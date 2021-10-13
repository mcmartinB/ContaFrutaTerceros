unit CFDContabilizacion_;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ActnList, StdCtrls, Db, DBTables, ComCtrls, BEdit, BSpeedButton,
  ExtCtrls, nbLabels, Math, BGridButton, BCalendarButton, BCalendario,
  Grids, DBGrids, BGrid, IdMessage, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdMessageClient, IdSMTP;

const
  kExtension= '.TXT';

type
  TDFDContabilizacion = class(TForm)
    Calendario: TBCalendario;
    lblMensaje: TLabel;
    IdSMTP: TIdSMTP;
    IdMessage: TIdMessage;
    nbLabel1: TnbLabel;
    cbxSerieFactura: TComboBox;
    eEmpresaAnyoFactura: TBEdit;
    eFacturaDesde: TBEdit;
    nbLabel2: TnbLabel;
    eFacturaHasta: TBEdit;
    btnContabilizar: TButton;
    btnCerrar: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure cbxSerieFacturaChange(Sender: TObject);
    procedure btnContabilizarClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

  private
    sEmpresa: String;
    dFechaIni, dFechaFin: TDateTime;
    iFacturaIni, iFacturaFin: integer;

    sEmailAdmi, sDirConta: string;
    sFileNameOk, sFileNameErr, sFileLog: string;
    slFichero: TStringList;

    iBaseFactura: integer;

    procedure GetFileNames( const ADir: string);
    procedure ContabilizarFacturas;
    procedure MarcarFacturas;
    function  HayErrores: Integer;

    function  FacturasParaContabilizar: integer;
    procedure ContabilizaFacturas;
    procedure CrearFichero;

    procedure EnviaCorreo( const ATitulo, ATexto: string );
    procedure AnyadirLog( const ATexto: string );
  public

  end;

var
 DFDContabilizacion: TDFDContabilizacion;

implementation

uses FileCtrl, kbmMemTable, UAutocontabilizar;

{$R *.DFM}

procedure TDFDContabilizacion.FormCreate(Sender: TObject);
begin

  sFileLog:= StringReplace( Application.ExeName, '.exe', '.log', [] );
  slFichero:= TStringList.Create;

  try
    DMDContabilizacion:= TDMDContabilizacion_.Create( self );
    DMDContabilizacion.Database.Open;
    DMDContabilizacion.PreparaQuerys;
  except
    on e: exception do
    begin
      EnviaCorreo( 'CONTABILIZACIÓN Error abrir base datos.', 'No se puede abrir la base de datos.' +  #13 + #10 +
                    e.Message );
      AnyadirLog( 'ERROR No se puede abrir la base de datos.' +  #13 + #10 + e.Message);
      Exit;
    end;
  end;

  //Obtener directorio
  DMDContabilizacion_.QDirectorioConta.Open;
  sDirConta := DMDContabilizacion_.QDirectorioConta.Fields[0].AsString;
  DMDContabilizacion_.QDirectorioConta.Close;

  if Trim( sDirConta ) = '' then
  begin
    EnviaCorreo( 'CONTABILIZACIÓN Ruta inexistente.',
                 'No se ha grabado la ruta donde guardar el fichero con la contabilización de las facturas.' + #13 + #10 +
                 ' -> Tabla cnf_directorios; valores ("conta_facturas","all","directorio_destino")');
    AnyadirLog( 'ERROR No se ha grabado la ruta donde guardar el fichero con la contabilización de las facturas.' + #13 + #10 +
                 ' -> Tabla cnf_directorios; valores ("conta_facturas","all","directorio_destino")');
    Exit;
  end;

  //Comprobar que podemos guardar fichero
  GetFileNames( sDirConta );

  //Obtener email
  DMDContabilizacion_.QEmailAdmi.Open;
  sEmailAdmi := DMDContabilizacion_.QEmailAdmi.Fields[0].AsString;
  DMDContabilizacion_.QEmailAdmi.Close;

  sEmpresa:= '050';
  dFechaIni:= Date - 356;
  dFechaFin:= Date - 7;
  iFacturaIni:= 0;
  iFacturaFin:= 999999;
end;

procedure TDFDContabilizacion.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  FreeAndNil( slFichero );
  DMDContabilizacion_.Database.Close;
  FreeAndNil( DMDContabilizacion_ );
end;

procedure TDFDContabilizacion.GetFileNames( const ADir: string );
var
  sAux: string;
begin
  sAux:= FormatDateTime( 'yyyymmdd_hhmmsszzzz', Now ) + kExtension;
  sFileNameOk:= ADir + '\BIC' + sAux;
  sFileNameErr:= ADir + '\ERR' + sAux;
end;

function TDFDContabilizacion.FacturasParaContabilizar: integer;
begin
  result:= DMDContabilizacion_.SeleccionarFacturas( sEmpresa, dFechaIni, dFechaFin, iFacturaIni, iFacturaFin );
end;

procedure TDFDContabilizacion.ContabilizaFacturas;
begin
  //Contabilizar facturas
  DMDContabilizacion_.QDocs.First;
  while not DMDContabilizacion_.QDocs.Eof do
  begin
    try
      If not DMDContabilizacion_.Database.InTransaction then
      begin
        DMDContabilizacion_.Database.StartTransaction;
        if DMDContabilizacion_.QDocs.FieldByName('tipo_factura_f').AsString = '380' then
        begin
          //Factura
          DMDContabilizacion_.ContabilizarFactura( ExtractFileName( sFileNameOk ) );
        end
        else
        begin
          //Abono
          DMDContabilizacion_.ContabilizarAbono( ExtractFileName( sFileNameOk ) );
        end;
        DMDContabilizacion_.DocContabilizado;
        DMDContabilizacion_.Database.Commit;
      end;
    except
      on e: exception do
      begin
        DMDContabilizacion_.Database.Rollback;
        DMDContabilizacion_.DocNoContabilizado( e.Message );
      end;
      //Seguir con el resto de facturas
    end;
    DMDContabilizacion_.QDocs.Next;
  end;
  DMDContabilizacion_.QDocs.Close;
end;

procedure TDFDContabilizacion.CrearFichero;
begin
  DMDContabilizacion_.FicheroTexto( ExtractFileName( sFileNameOk ), slFichero );
end;

procedure TDFDContabilizacion.ContabilizarFacturas;
var
  iFacturas, iErrores: integer;
  sAux: TStringList;
begin
  sAux:= TStringList.Create;
  sAux.Add(' Inicio contabilización ' + DateTimeToStr( Now ) );
  iFacturas:= FacturasParaContabilizar;
  if iFacturas > 0  then
  begin
    try
      slFichero.SaveToFile( sFileNameOk );
    except
      EnviaCorreo( 'CONTABILIZACIÓN Error crear fichero.', 'No se puede crear el fichero ' + sFileNameOk );
      AnyadirLog( 'ERROR No se puede crear el fichero ' + sFileNameOk );
      Exit;
    end;


    sAux.Add( ' ' + IntToStr( iFacturas )  + ' FACTURAS ');

    ContabilizaFacturas;
    CrearFichero;

    slFichero.SaveToFile( sFileNameOk );
    //Despues de grabar el fichero correctamente
    MarcarFacturas;
    iErrores:= HayErrores;
    if iErrores > 0 then
    begin
      sAux.Add('  ' + IntToStr( iFacturas - iErrores )  + ' CONTABILIZADAS ');
      sAux.Add('  ' + IntToStr( iErrores )  + ' NO CONTABILIZADAS ');
      sAux.Add(' Proceso finalizado con errores.');
      sAux.Add(' FICHERO CONTABILIZACION = ' + ExtractFileName( sFileNameOk ) );
      sAux.Add(' FICHERO ERRORES = ' + ExtractFileName( sFileNameErr ) );
    end
    else
    begin
      sAux.Add(' Proceso finalizado con éxito.');
      sAux.Add(' FICHERO CONTABILIZACION = ' + ExtractFileName( sFileNameOk ) );
    end;
  end
  else
  begin
    sAux.Add(' No hay facturas a contabilizar.');
  end;

  sAux.Add(' Fin contabilización ' + DateTimeToStr( Now ) );

  EnviaCorreo( 'CONTABILIZACIÓN OK', sAux.Text);
  AnyadirLog( sAux.Text );

  FreeAndNil(sAux);
end;

function TDFDContabilizacion.HayErrores: integer;
begin
  with DMDContabilizacion_.mtNoContabilizadas do
  begin
    Open;
    result:= RecordCount;
    if result > 0 then
      SaveToFile( sFileNameErr, [mtfSaveData] );
    Close;
  end;
end;

procedure TDFDContabilizacion.MarcarFacturas;
begin
  with DMDContabilizacion_.mtContabilizadas do
  begin
    Open;
    First;
    while not EOF do
    begin
      DMDContabilizacion_.QMarcarFactura.ParamByName('empresa').AsString:=
        FieldByName('empresa').AsString;
      DMDContabilizacion_.QMarcarFactura.ParamByName('factura').AsInteger:=
        FieldByName('factura').AsInteger;
      DMDContabilizacion_.QMarcarFactura.ParamByName('fecha').AsDate:=
        FieldByName('fecha').AsDateTime;
      DMDContabilizacion_.QMarcarFactura.ParamByName('filename').AsString:=
        ExtractFileName( sFileNameOk );
      DMDContabilizacion_.QMarcarFactura.ExecSQL;
      Next;
    end;
    Close;
  end;
end;

procedure TDFDContabilizacion.EnviaCorreo( const ATitulo, ATexto: string );
begin
  if not IdSMTP.Connected then
  begin
    try
      IdSMTP.Connect;
    except
      //Error al conectar LOG
      AnyadirLog( 'ERROR al intentar conectar con el servidor de correo.' );
      Exit;
    end;
  end;

  try
    try
      Screen.Cursor := crHourGlass;
      IdMessage.From.Address := 'contabilizar@facturas.es';
      IdMessage.Recipients.Add.Address:= 'pepebrotons@bonnysa.es';
      IdMessage.Subject := ATitulo;
      IdMessage.Body.Add( ATexto );
      IdSMTP.Send(IdMessage);
    finally
      IdSMTP.Disconnect;
      Screen.Cursor := crDefault;
    end;
  except
   //Error al enviar LOG
   AnyadirLog( 'ERROR al enviar el mensaje de correo.' );
   Exit;
  end;
end;

procedure TDFDContabilizacion.AnyadirLog( const  ATexto: string );
var
  slAux: TStringList;
begin
  slAux:= TStringList.Create;
  if FileExists( sFileLog ) then
  begin
    slAux.LoadFromFile( sFileLog );
  end;
  slAux.Add('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
  slAux.Add( DateTimeToStr( Now ) );
  slAux.Add('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
  slAux.Add( ATexto );
  slAux.Add('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<');
  try
    try
      slAux.SaveToFile( sFileLog );
    except
      //No hacemos nada
    end;
  finally
    FreeAndNil( slAux );
  end;

end;

procedure TDFDContabilizacion.FormShow(Sender: TObject);
var
  iAnyo, iMes, iDia: word;
begin
  if bAu
  //ContabilizarFacturas;
  lblMensaje.Caption:= 'Facturas serie IVA.';
  iBaseFactura:= 100000;
  DecodeDate( Date, iAnyo, iMes, iDia );
  eEmpresaAnyoFactura.Text:= '050' + Copy( IntToStr( iAnyo ), 3, 2);
  eFacturaDesde.Text:= '0';
  eFacturaHasta.Text:= '0';
end;

procedure TDFDContabilizacion.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

procedure TDFDContabilizacion.cbxSerieFacturaChange(Sender: TObject);
begin
  case cbxSerieFactura.ItemIndex of
    0:lblMensaje.Caption:= 'Facturas serie IVA.';
    1:lblMensaje.Caption:= 'Facturas serie IGIC.';
    2:lblMensaje.Caption:= 'Abonos serie IVA.';
    3:lblMensaje.Caption:= 'Abonos serie IGIC.';
  end;
  iBaseFactura:= ( cbxSerieFactura.ItemIndex + 1 )* 100000;
end;

procedure TDFDContabilizacion.btnContabilizarClick(Sender: TObject);
var
  iAnyoAux: word;
  iAnyo, iMes, iDia: word;
begin
  if Length(Trim(eEmpresaAnyoFactura.Text))<5 then
  begin
    ShowMessage('Empresa/Año incorrecto (FORMATO: "EEEAA").');
    eEmpresaAnyoFactura.Focused;
    Exit;
  end;
  sEmpresa:= copy( eEmpresaAnyoFactura.Text, 1, 3 );

  iAnyoAux:= StrToInt( copy( eEmpresaAnyoFactura.Text, 4, 2 ) ) + 2000;
  DecodeDate( date, iAnyo, iMes, iDia );
  if iAnyoAux > iAnyo then
  begin
    ShowMessage('Empresa/Año incorrecto (FORMATO: "EEEAA").');
    eEmpresaAnyoFactura.Focused;
    Exit;
  end;
  if iAnyoAux = iAnyo then
  begin
    iDia:= 1;
    iMes:= 1;
    dFechaIni:= EncodeDate( iAnyoAux, iMes, iDia );
    dFechaFin:= Date;
  end
  else
  begin
    iDia:= 1;
    iMes:= 1;
    dFechaIni:= EncodeDate( iAnyoAux, iMes, iDia );
    iDia:= 31;
    iMes:= 12;
    dFechaFin:= EncodeDate( iAnyoAux, iMes, iDia );
  end;


  if Trim(eFacturaDesde.Text) = '' then
  begin
    ShowMessage('Falta la factura inicial.');
    eFacturaDesde.Focused;
    Exit;
  end;
  iFacturaIni:= StrToInt( eFacturaDesde.Text ) + iBaseFactura;

  if Trim(eFacturaHasta.Text) = '' then
  begin
    ShowMessage('Falta la factura final.');
    eFacturaHasta.Focused;
    Exit;
  end;
  iFacturaFin:= StrToInt( eFacturaHasta.Text ) + iBaseFactura;

  ContabilizarFacturas;
end;

procedure TDFDContabilizacion.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    vk_Return, vk_down:
      begin
        Key := 0;
        PostMessage(Handle, WM_NEXTDLGCTL, 0, 0);
      end;
    vk_up:
      begin
        Key := 0;
        PostMessage(Handle, WM_NEXTDLGCTL, 1, 0);
      end;
    vk_f1:
      begin
        Key := 0;
        btnContabilizar.Click;
      end;
    vk_escape:
      begin
        Key := 0;
        btnCerrar.Click;
      end;
  end;
end;

end.
