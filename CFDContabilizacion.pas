unit CFDContabilizacion;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ActnList, StdCtrls, Db, DBTables, ComCtrls, BEdit, BSpeedButton,
  ExtCtrls, nbLabels, BGridButton, BCalendarButton, BCalendario,
  Grids, DBGrids, BGrid, IdMessage, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdMessageClient, IdSMTP;

const
  kExtension= '.TXT';

type
  TDFDContabilizacion = class(TForm)
    Calendario: TBCalendario;
    lblMsg: TLabel;
    lblFacturaDesde: TnbLabel;
    lblFacturaHasta: TnbLabel;
    btnContabilizar: TButton;
    btnCerrar: TButton;
    eDesde: TDateTimePicker;
    eHasta: TDateTimePicker;
    nbLabel1: TnbLabel;
    eEmpresa: TEdit;
    stEmpresa: TStaticText;
    QEmpresa: TQuery;
    nbLabel2: TnbLabel;
    eFileName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    QDirectorioConta: TQuery;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnContabilizarClick(Sender: TObject);
    procedure eEmpresaChange(Sender: TObject);

  private
    sEmpresa: String;
    dFechaIni, dFechaFin: TDateTime;

    sDirConta, sFileNameOK, sFileNameLis: string;

    procedure GetFileNames( const ADir: string);
    function  CargaParametros: boolean;
    procedure Contabilizar;
  public

  end;

var
 DFDContabilizacion: TDFDContabilizacion;

implementation

uses FileCtrl, kbmMemTable, CMDContabilizacion, CFDClave;

{$R *.DFM}

procedure TDFDContabilizacion.FormCreate(Sender: TObject);
var
  iAnyo, iMes, iDia: word;
  sUsuario, sClave: string;
begin
  DMDContabilizacion:= TDMDContabilizacion.Create( self );
  if CFDClave.Ejecutar( sUsuario, sClave ) then
  begin
    DMDContabilizacion.Database.Params.Values['USER NAME']:= sUsuario;
    DMDContabilizacion.Database.Params.Values['PASSWORD']:= sClave;
    try
      DMDContabilizacion.Database.Open
    except
      ShowMessage('Error al conectar con la base de datos.');
      Exit;
    end;
  end
  else
  begin
    Exit;
  end;
  with QEmpresa do
  begin
    SQL.Clear;
    SQL.Add('select nombre_e ');
    SQL.Add('from frf_empresas ');
    SQL.Add('where empresa_e = :empresa ');
    Prepare;
  end;

  with QDirectorioConta do
  begin
    SQL.Clear;
    SQL.Add(' select directorio_cd ');
    SQL.Add(' from cnf_directorios ');
    SQL.Add(' where UPPER(codigo_cd) = ''CONTA_XGAS3'' ');
    SQL.Add(' and UPPER(usuario_cd) = ''ALL'' ');
  end;

  //Obtener directorio
  QDirectorioConta.Open;
  sDirConta := QDirectorioConta.Fields[0].AsString;
  QDirectorioConta.Close;


  if Trim( sDirConta ) = '' then
  begin
    ShowMessage('No se ha grabado la ruta donde guardar el fichero con la contabilización de las facturas.' + #13 + #10 +
                ' -> Tabla cnf_directorios; valores ("conta_facturas","all","directorio_destino")');
    Exit;
  end;

  DecodeDate( Date, iAnyo, iMes, iDia );
  eEmpresa.Text:= '050';
  eDesde.Date:= StrToDate('1/7/2015');
  eHasta.Date:= StrToDate('1/8/2015');
end;

procedure TDFDContabilizacion.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if QEmpresa.Prepared then
     QEmpresa.UnPrepare;

  Action := caFree;
  DMDContabilizacion.Database.Close;
  FreeAndNil( DMDContabilizacion );
end;

procedure TDFDContabilizacion.GetFileNames( const ADir: string );
var
  sAux: string;
begin
  if eFileName.Text <> '' then
    sAux:= eFileName.Text + kExtension
  else
    sAux:= FormatDateTime( 'yyyymmdd_hhmmsszzzz', Now ) + kExtension;
  sFileNameOk:= ADir + '\GAS3' + sAux;
  sFileNameLis:=ADir + '\GAS3_Listado.csv';
end;


procedure TDFDContabilizacion.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

function  TDFDContabilizacion.CargaParametros: boolean;
begin
  result:= False;
  if stEmpresa.Caption = '' then
  begin
    ShowMessage('Empresa incorrecta.');
    eEmpresa.SetFocus;
    Exit;
  end;
  sEmpresa:= eEmpresa.Text;

  dFechaIni:= eDesde.Date;
  dFechaFin:= eHasta.Date;
  if dFechaFin < dFechaIni then
  begin
    ShowMessage('Rango de fechas incorrecto.');
    eDesde.SetFocus;
    Exit;
  end;
  result:= True;
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

procedure TDFDContabilizacion.btnContabilizarClick(Sender: TObject);
begin
  Contabilizar;
end;

procedure TDFDContabilizacion.Contabilizar;
var
  iFacturas: integer;
  sAux: TStringList;
  sMsg: string;
begin
  lblMsg.Caption:= 'Recontabilizando facturas, espere por favor ...';
  sMsg:= '';

  if CargaParametros then
  begin
    GetFileNames( sDirConta );
    DMDContabilizacion.PreparaQuerys( sEmpresa );

    sAux:= TStringList.Create;
    sAux.Add(' Inicio contabilización ' + DateTimeToStr( Now ) );
    iFacturas:= DMDContabilizacion.SeleccionarFacturas( sEmpresa, dFechaIni, dFechaFin );
    if iFacturas > 0  then
    begin
      sAux.Add( ' ' + IntToStr( iFacturas )  + ' FACTURAS ');

      if DMDContabilizacion.ContabilizaFacturas( ExtractFileName( sFileNameOk ) , sMsg ) then
      begin
        DMDContabilizacion.slListado.SaveToFile( sFileNameLis );
        if DMDContabilizacion.FicheroTexto( sFileNameOk ) then
          WinExec( Pchar( 'notepad ' + sFileNameOk ), SW_SHOWNORMAL );
        lblMsg.Caption:= ' Proceso finalizado con éxito.';
      end
      else
      begin
        lblMsg.Caption:= ' Proceso abortado.';
      end;
    end
    else
    begin
      lblMsg.Caption:= ' No hay facturas a contabilizar.';
    end;

    sAux.Add( lblMsg.Caption );
    if sMsg <> '' then
      sAux.Add( sMsg );
    sAux.Add(' Fin contabilización ' + DateTimeToStr( Now ) );
    eFileName.Text:= '';

    ShowMessage( sAux.Text );
    FreeAndNil(sAux);
  end;
end;

procedure TDFDContabilizacion.eEmpresaChange(Sender: TObject);
begin
  if Length(Trim(eEmpresa.Text)) = 3 then
  begin
    with QEmpresa do
    begin
      ParamByName('empresa').AsString:= eEmpresa.Text;
      Open;
      stEmpresa.Caption:= FieldByName('nombre_e').AsString;
      Close;
    end;
  end
  else
  begin
    stEmpresa.Caption:= '';
  end;
end;



end.

