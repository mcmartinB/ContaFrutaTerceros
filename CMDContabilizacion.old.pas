unit CMDContabilizacion;

interface

uses
  SysUtils, Classes, DB, DBTables, kbmMemTable;

type
  TRSeccion = record
    seccion: string;
    importe: real;
    gastosFac: real;
    unidad: string;
    unidades: real;
    importeNeto: real;
  end;

  TDMDContabilizacion = class(TDataModule)
    QDocs: TQuery;
    QNumDocs: TQuery;
    QTercero: TQuery;
    QCambio: TQuery;
    QSalidas: TQuery;
    QSecciones: TQuery;
    QGastos: TQuery;
    mtContabilizadas: TkbmMemTable;
    mtNoContabilizadas: TkbmMemTable;
    QMarcarFactura: TQuery;
    QDirectorioConta: TQuery;
    QFormaPago: TQuery;
    QFacturaAsociada: TQuery;
    facturas_conta_t: TQuery;
    facturas_conta_d: TQuery;
    facturas_conta_e: TQuery;
    facturas_conta_a: TQuery;
    aux_facturas_conta_t1: TQuery;
    DSAUX_facturas_conta: TDataSource;
    aux_facturas_conta_d: TQuery;
    aux_facturas_conta_a: TQuery;
    aux_facturas_conta_e: TQuery;
    aux_facturas_conta_t2: TQuery;
    DSAUX_facturas_conta_d: TDataSource;
    Database: TDatabase;
    QEmailAdmi: TQuery;
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    aSecciones: array of TRSeccion;
    bPeninsula: boolean;
    sFactura, sAbono: string;
    sCtaCliente, sDirTercero, sCtaPGC, sCtaPGA, sTipoIva, sRegimenIva, sFormaPago: string;
    rImporteNetoFactura, rDescuento, rComision: real;
    rGastosPalet, rGastosCaja, rGastosKilo, rGastosImporte: real;
    rGastosTPalet, rGastosTCaja, rGastosTKilo, rGastosTImporte: real;
    iIncDias, iDiaPago1, iDiaPago2: integer;

    function  FechaAdonix( const AFecha: TDateTime ): string;
    function  Planta: string;
    function  Direccion: string;
    procedure Tercero;
    function  FechaVencimiento: TDateTime;
    function  Divisa: string;
    function  CambioDivisa: string;
    function  FacturaAsociada: String;

    function  AnyadirSecciones: boolean;
    procedure PutSeccion( const ASeccion, AUnidad: string; const AImporte, AGastosFac, AUnidades: Real);
    procedure SeccionesSalida;
    procedure ObtenerGastos;

    procedure InsertarLineaT( const AFileName: string );
    procedure InsertarLineaD( const ALinea: integer );
    procedure InsertarLineaA( const ALinea: integer );
    procedure InsertarLineaE;

    procedure ContaFacturaToAbono( const AFileName: string );
    procedure ContaFacturaToAbonoT( const AFileName: string );
    procedure ContaFacturaToAbonoD;
    procedure ContaFacturaToAbonoA;
    procedure ContaFacturaToAbonoE;

    procedure AddFacturaFicheroTexto( var ATexto: TStringList );

  public
    { Public declarations }
    procedure PreparaQuerys;
    function  SeleccionarFacturas( const AEmpresa: string; const AFechaIni, AFechaFin: TDateTime;
                                 const AFacturaIni, AFacturaFin: integer ): integer;

    procedure ContabilizarFactura( const AFileName: string );
    procedure ContabilizarAbono( const AFileName: string );
    procedure FicheroTexto( const AFileName: string; var ATexto: TStringList );
    procedure DocContabilizado;
    procedure DocNoContabilizado( const AError: string );

  end;

var
  DMDContabilizacion: TDMDContabilizacion;

implementation

{$R *.dfm}

uses Variants, Math, bTextUtils;

function NewContadorFactura( const AFactura: integer ): string;
begin
  if AFactura > 99999 then
  begin
    result:= IntToStr( AFactura );
    result:= Copy( result, length( result ) - 4, 5 );
  end
  else
  begin
    result:= Rellena( IntToStr( AFactura ), 5, '0', taLeftJustify );
  end;
end;

function GetPrefijoFactura( const AEmpresa, ATipo, AImpuesto: string; const AFechaFactura: TDateTime ): string;
var
  iAnyo, iMes, iDia: word;
  sAux: string;
begin
  result:= 'ERROR ';
  DecodeDate( AFechaFactura, iAnyo, iMes, iDia );
  sAux:= IntToStr( iAnyo );
  sAux:= Copy( sAux, 3, 2 );
  if Copy( AImpuesto, 1, 1 ) = 'I' then
  begin
    if ATipo = '380' then
    begin
      result:= 'FCP-' + AEmpresa + sAux + '-';
    end
    else
    begin
      result:= 'ACP-' + AEmpresa + sAux + '-';
    end;
  end
  else
  begin
    if ATipo = '380' then
    begin
      result:= 'FCT-' + AEmpresa + sAux + '-';
    end
    else
    begin
      result:= 'ACT-' + AEmpresa + sAux + '-';
    end;
  end;
end;

function  NewCodigoFactura( const AEmpresa, ATipo, AImpuesto: string;
                            const AFechaFactura: TDateTime;
                            const AFactura: integer ): string;
begin
  if ( AFechaFactura >= StrToDate( '1/7/2006' ) ) and ( AEmpresa = '050' ) then
  begin
    result:= GetPrefijoFactura( AEmpresa, ATipo, AImpuesto, AFechaFactura ) +
             NewContadorFactura( AFactura );
  end
  else
  begin
    if ( ATipo = '381' ) and ( AFechaFactura >= StrToDate('1/1/2006') ) then
    begin
        result:= 'AB' + FormatFloat( '00000', AFactura );
    end
    else
    begin
      result:= IntToStr( AFactura );
    end;
  end;
end;

procedure TDMDContabilizacion.PreparaQuerys;
begin
  with QNumDocs do
  begin
    SQL.Clear;
    SQL.Add('select count(*) ');
    SQL.Add('from frf_facturas');
    SQL.Add('where empresa_f = :empresa');
    SQL.Add('and fecha_factura_f between :fechaini and :fechafin');
    SQL.Add('and n_factura_f between :facturaini and :facturafin');
    SQL.Add('and  NVL( contabilizado_f, ''N'' ) = ''N''  ');
    //Factura/Abono automatica
    //SQL.Add('and  tipo_factura_f = 380 ');
    SQL.Add('and  concepto_f = ''A'' ');
    Prepare;
  end;

  with QDocs do
  begin
    SQL.Clear;
    SQL.Add('select * ');
    SQL.Add('from frf_facturas');
    SQL.Add('where empresa_f = :empresa');
    SQL.Add('and fecha_factura_f between :fechaini and :fechafin');
    SQL.Add('and n_factura_f between :facturaini and :facturafin');
    SQL.Add('and  NVL( contabilizado_f, ''N'' ) = ''N''  ');
    SQL.Add('and  concepto_f = ''A'' ');
    SQL.Add('order by tipo_factura_f ');
    Prepare;
  end;

  with QTercero do
  begin
    SQL.Clear;
    SQL.Add('select cta_cliente_c, cta_ingresos_pgc_c, cta_ingresos_pga_c,');
    SQL.Add('       forma_pago_c, dia_vencim1_c, dia_vencim2_c,');
    SQL.Add('       nvl(case when porc_dto_fac_c <> 0 then porc_dto_c else 0 end, 0) descuento_c,');
    SQL.Add('       nvl( comision_r, 0 ) comision_c');
    SQL.Add('from frf_clientes, outer frf_representantes');
    SQL.Add('where empresa_c = :empresa');
    SQL.Add('and cliente_c = :cliente');
    SQL.Add('and empresa_r = :empresa');
    SQL.Add('and representante_r = representante_c');
    Prepare;
  end;

  with QCambio do
  begin
    SQL.Clear;
    SQL.Add(' select cambio_ce ');
    SQL.Add(' from frf_cambios_euros ');
    SQL.Add(' where moneda_ce = :moneda ');
    SQL.Add(' and fecha_ce = :fecha ');
    Prepare;
  end;

  with QSalidas do
  begin
    SQL.Clear;
    SQL.Add(' select empresa_sc, centro_salida_sc, n_albaran_sc, fecha_sc, dir_sum_sc,');

    SQL.Add('        sum(nvl(n_palets_sl,0)) palets_sc, ');
    SQL.Add('        sum(nvl(cajas_sl,0)) cajas_sc, ');
    SQL.Add('        sum(nvl(kilos_sl,0)) kilos_sc, ');
    SQL.Add('        sum(nvl(importe_neto_sl,0)) neto_sc, ');

    SQL.Add('        sum(case when ref_transitos_sl is not null then nvl(n_palets_sl,0) else 0 end) palets_tran_sc, ');
    SQL.Add('        sum(case when ref_transitos_sl is not null then nvl(cajas_sl,0) else 0 end) cajas_tran_sc, ');
    SQL.Add('        sum(case when ref_transitos_sl is not null then nvl(kilos_sl,0) else 0 end) kilos_tran_sc, ');
    SQL.Add('        sum(case when ref_transitos_sl is not null then nvl(importe_neto_sl,0) else 0 end) neto_tran_sc ');

    SQL.Add(' from frf_salidas_c, frf_salidas_l ');
    SQL.Add(' where empresa_sc = :empresa ');
    SQL.Add(' and n_factura_sc = :factura ');
    SQL.Add(' and fecha_factura_sc = :fecha ');
    SQL.Add(' and empresa_sl = :empresa ');
    SQL.Add(' and centro_salida_sl = centro_salida_sc ');
    SQL.Add(' and n_albaran_sl = n_albaran_sc ');
    SQL.Add(' and fecha_sl = fecha_sc ');
    SQL.Add(' group by empresa_sc, centro_salida_sc, n_albaran_sc, fecha_sc, dir_sum_sc ');
    SQL.Add(' order by fecha_sc, n_albaran_sc');
    Prepare;
  end;

  with QSecciones do
  begin
    SQL.Clear;
    SQL.Add(' select sec_contable_sc seccion_s, ');

    SQL.Add(' sum(nvl(n_palets_sl,0)) palets_s, ');
    SQL.Add(' sum(nvl(cajas_sl,0)) cajas_s, ');
    SQL.Add(' sum(nvl(kilos_sl,0)) kilos_s, ');
    SQL.Add(' sum(nvl(importe_neto_sl,0)) neto_s, ');

    SQL.Add(' sum(case when ref_transitos_sl is not null then nvl(n_palets_sl,0) else 0 end) palets_tran_s, ');
    SQL.Add(' sum(case when ref_transitos_sl is not null then nvl(cajas_sl,0) else 0 end) cajas_tran_s, ');
    SQL.Add(' sum(case when ref_transitos_sl is not null then nvl(kilos_sl,0) else 0 end) kilos_tran_s, ');
    SQL.Add(' sum(case when ref_transitos_sl is not null then nvl(importe_neto_sl,0) else 0 end) neto_tran_s ');

    SQL.Add(' from frf_salidas_l, outer frf_secc_contables ');
    SQL.Add(' where empresa_sl = :empresa ');
    SQL.Add(' and centro_salida_sl = :centro ');
    SQL.Add(' and n_albaran_sl = :albaran ');
    SQL.Add(' and fecha_sl = :fecha ');
    SQL.Add(' and empresa_sc = emp_procedencia_sl ');
    SQL.Add(' and producto_sc = producto_sl ');
    SQL.Add(' and centro_sc = centro_origen_sl ');
    SQL.Add(' group by 1 ');
    Prepare;
  end;

  with QGastos do
  begin
    SQL.Clear;
    SQL.Add(' select nvl(gasto_transito_tg,0) transito_g, unidad_dist_tg unidad_g, sum(importe_g) importe_g');
    SQL.Add(' from frf_gastos, frf_tipo_gastos ');
    SQL.Add(' where empresa_g = :empresa ');
    SQL.Add(' and centro_salida_g = :centro ');
    SQL.Add(' and n_albaran_g = :albaran ');
    SQL.Add(' and fecha_g = :fecha ');
    SQL.Add(' and tipo_tg = tipo_g ');
    SQL.Add(' and facturable_tg = ''S'' ');
    SQL.Add(' group by 1, 2 ');
    Prepare;
  end;

  mtContabilizadas := TkbmMemTable.Create(Self);
  mtContabilizadas.FieldDefs.Clear;
  mtContabilizadas.FieldDefs.Add('empresa', ftString, 3, False);
  mtContabilizadas.FieldDefs.Add('factura', ftInteger, 0, False);
  mtContabilizadas.FieldDefs.Add('fecha', ftDateTime, 0, False);
  mtContabilizadas.CreateTable;
  mtContabilizadas.SortFields := 'empresa, factura, fecha';

  mtNoContabilizadas := TkbmMemTable.Create(Self);
  mtNoContabilizadas.FieldDefs.Clear;
  mtNoContabilizadas.FieldDefs.Add('empresa', ftString, 3, False);
  mtNoContabilizadas.FieldDefs.Add('factura', ftInteger, 0, False);
  mtNoContabilizadas.FieldDefs.Add('fecha', ftDateTime, 0, False);
  mtNoContabilizadas.FieldDefs.Add('error', ftString, 255, False);
  mtNoContabilizadas.CreateTable;
  mtNoContabilizadas.SortFields := 'empresa, factura, fecha';

  with QMarcarFactura do
  begin
    SQL.Clear;
    SQL.Add('update frf_facturas');
    SQL.Add('set contabilizado_f = ''S'', ');
    SQL.Add('    filename_conta_f = :filename ');
    SQL.Add('where empresa_f = :empresa');
    SQL.Add('and fecha_factura_f = :fecha');
    SQL.Add('and n_factura_f = :factura');
    Prepare;
  end;

  with QDirectorioConta do
  begin
    SQL.Clear;
    SQL.Add(' select directorio_d ');
    SQL.Add(' from cnf_directorios ');
    SQL.Add(' where UPPER(codigo_d) = ''CONTA_FACTURAS'' ');
    SQL.Add(' and UPPER(usuario_d) = ''ALL'' ');
    Prepare;
  end;

  with QEmailAdmi do
  begin
    SQL.Clear;
    SQL.Add(' select email_e ');
    SQL.Add(' from cnf_emails ');
    SQL.Add(' where UPPER(codigo_e) = ''ADMINISTRACION'' ');
    SQL.Add(' and UPPER(usuario_e) = ''ADMI'' ');
    Prepare;
  end;

  with QFormaPago do
  begin
    SQL.Clear;
    SQL.Add(' select * ');
    SQL.Add(' from frf_forma_pago ');
    SQL.Add(' where codigo_fp = :codigo ');
    Prepare;
  end;

  with QFacturaAsociada do
  begin
    SQL.Clear;
    SQL.Add(' select empresa_fa, n_factura_fa, fecha_factura_fa ');
    SQL.Add(' from frf_facturas_abono ');
    SQL.Add(' where empresa_fa = :empresa ');
    SQL.Add(' and n_abono_fa = :abono ');
    SQL.Add(' and fecha_abono_fa = :fecha ');
    Prepare;
  end;


  with facturas_conta_t do
  begin
    SQL.Clear;
    SQL.Add(' select * ');
    SQL.Add(' from frf_facturas_conta_t ');
    RequestLive:= True;
    Prepare;
  end;
  with facturas_conta_d do
  begin
    SQL.Clear;
    SQL.Add(' select * ');
    SQL.Add(' from frf_facturas_conta_d ');
    RequestLive:= True;
    Prepare;
  end;
  with facturas_conta_a do
  begin
    SQL.Clear;
    SQL.Add(' select * ');
    SQL.Add(' from frf_facturas_conta_a ');
    RequestLive:= True;
    Prepare;
  end;
  with facturas_conta_e do
  begin
    SQL.Clear;
    SQL.Add(' select * ');
    SQL.Add(' from frf_facturas_conta_e ');
    RequestLive:= True;
    Prepare;
  end;

  with aux_facturas_conta_t1 do
  begin
    SQL.Clear;
    SQL.Add(' select * ');
    SQL.Add(' from frf_facturas_conta_t ');
    SQL.Add(' where empresa_fct = :empresa ');
    SQL.Add('   and n_factura_fct = :factura ');
    SQL.Add('   and fecha_factura_fct = :fecha ');
    Prepare;
  end;
  with aux_facturas_conta_t2 do
  begin
    SQL.Clear;
    SQL.Add(' select * ');
    SQL.Add(' from frf_facturas_conta_t ');
    SQL.Add(' where nombre_fichero_fct = :fichero ');
    Prepare;
  end;
  with aux_facturas_conta_d do
  begin
    SQL.Clear;
    SQL.Add(' select * ');
    SQL.Add(' from frf_facturas_conta_d ');
    SQL.Add(' where numero_fcd = :numero_fct ');
    Prepare;
  end;
  with aux_facturas_conta_a do
  begin
    SQL.Clear;
    SQL.Add(' select * ');
    SQL.Add(' from frf_facturas_conta_a ');
    SQL.Add(' where numero_fca = :numero_fcd ');
    SQL.Add('   and linea_fca = :linea_fcd ');
    Prepare;
  end;
  with aux_facturas_conta_e do
  begin
    SQL.Clear;
    SQL.Add(' select * ');
    SQL.Add(' from frf_facturas_conta_e ');
    SQL.Add(' where numero_fce = :numero_fct ');
    Prepare;
  end;
end;

procedure TDMDContabilizacion.DataModuleDestroy(Sender: TObject);
begin
  if Database.Connected then
  begin
    QNumDocs.Close;
    if QNumDocs.Prepared then
      QNumDocs.UnPrepare;
    QDocs.Close;
    if QDocs.Prepared then
      QDocs.UnPrepare;

    QTercero.Close;
    if QTercero.Prepared then
      QTercero.UnPrepare;
    QCambio.Close;
    if QCambio.Prepared then
      QCambio.UnPrepare;
    QSalidas.Close;
    if QSalidas.Prepared then
      QSalidas.UnPrepare;
    QSecciones.Close;
    if QSecciones.Prepared then
      QSecciones.UnPrepare;
    QGastos.Close;
    if QGastos.Prepared then
      QGastos.UnPrepare;
    QMarcarFactura.Close;
    if QMarcarFactura.Prepared then
      QMarcarFactura.UnPrepare;
    QFormaPago.Close;
    if QFormaPago.Prepared then
      QFormaPago.UnPrepare;
    QFacturaAsociada.Close;
    if QFacturaAsociada.Prepared then
      QFacturaAsociada.UnPrepare;

    facturas_conta_e.Close;
    if facturas_conta_e.Prepared then
      facturas_conta_e.UnPrepare;
    facturas_conta_a.Close;
    if facturas_conta_a.Prepared then
      facturas_conta_a.UnPrepare;
    facturas_conta_d.Close;
    if facturas_conta_d.Prepared then
      facturas_conta_d.UnPrepare;
    facturas_conta_t.Close;
    if facturas_conta_t.Prepared then
      facturas_conta_t.UnPrepare;

    aux_facturas_conta_e.Close;
    if aux_facturas_conta_e.Prepared then
      aux_facturas_conta_e.UnPrepare;
    aux_facturas_conta_a.Close;
    if aux_facturas_conta_a.Prepared then
      aux_facturas_conta_a.UnPrepare;
    aux_facturas_conta_d.Close;
    if aux_facturas_conta_d.Prepared then
      aux_facturas_conta_d.UnPrepare;
    aux_facturas_conta_t1.Close;
    if aux_facturas_conta_t1.Prepared then
      aux_facturas_conta_t1.UnPrepare;
    aux_facturas_conta_t2.Close;
    if aux_facturas_conta_t2.Prepared then
      aux_facturas_conta_t2.UnPrepare;

    mtContabilizadas.Close;
    FreeAndNil( mtContabilizadas );
    mtNoContabilizadas.Close;
    FreeAndNil( mtNoContabilizadas );
  end;
end;

function TDMDContabilizacion.SeleccionarFacturas( const AEmpresa: string;
  const AFechaIni, AFechaFin: TDateTime; const AFacturaIni, AFacturaFin: integer ): integer;
begin
  with QNumDocs do
  begin
    ParamByName('empresa').AsString:= AEmpresa;
    ParamByName('fechaini').AsDate:= AFechaIni;
    ParamByName('fechafin').AsDate:= AFechaFin;
    ParamByName('facturaini').AsInteger:= AFacturaIni;
    ParamByName('facturafin').AsInteger:= AFacturaFin;
    Open;
    result:= Fields[0].AsInteger;
    Close;

    if result = 0 then
      Exit;
  end;

  with QDocs do
  begin
    ParamByName('empresa').AsString:= AEmpresa;
    ParamByName('fechaini').AsDate:= AFechaIni;
    ParamByName('fechafin').AsDate:= AFechaFin;
    ParamByName('facturaini').AsInteger:= AFacturaIni;
    ParamByName('facturafin').AsInteger:= AFacturaFin;
    Open;
  end;

  end;

function  TDMDContabilizacion.FechaAdonix( const AFecha: TDateTime ): string;
begin
  result:= StringReplace( DateToStr( AFecha ), '/', '', [rfReplaceAll, rfIgnoreCase] );
end;

function  TDMDContabilizacion.Planta: string;
begin
  result:= QDocs.FieldByName('empresa_f').AsString;
end;

function  TDMDContabilizacion.Direccion: string;
begin
  if copy( QDocs.FieldByName('tipo_impuesto_f').AsString, 1, 1 ) = 'I' then
    result:= '001'  //Peninsula
  else
    result:= '002'; //Tenerife
end;

procedure TDMDContabilizacion.Tercero;
var
  sAux: String;
begin
  sAux:= QDocs.FieldByName('cliente_fac_f').AsString;
  with QTercero do
  begin
    Close;
    ParamByName('empresa').AsString:= QDocs.FieldByName('empresa_f').AsString;
    ParamByName('cliente').AsString:= sAux;
    Open;

    sCtaCliente:= FieldByName('cta_cliente_c').AsString;
    sCtaPGC:= FieldByName('cta_ingresos_pgc_c').AsString;
    sCtaPGA:= FieldByName('cta_ingresos_pga_c').AsString;

    sDirTercero:= '001';

    rDescuento:= FieldByName('descuento_c').AsFloat;
    rComision:= FieldByName('comision_c').AsFloat;


    if FieldByName('forma_pago_c').AsString <> '' then
    begin
      QFormaPago.ParamByName('codigo').AsString:= FieldByName('forma_pago_c').AsString;
      QFormaPago.Open;
      sFormaPago:= QFormaPago.FieldByName('forma_pago_adonix_fp').AsString;
      (*TODO*)
      (*
      if sFormaPago = '' then
      begin
        raise Exception.Create('Falta la forma de pago ADONIX en el mantenimiento de Formas de Pago.');
      end;
      *)
      iIncDias:= QFormaPago.FieldByName('dias_cobro_fp').AsInteger;
      QFormaPago.Close;
    end
    else
    begin
      sFormaPago:= '';
      iIncDias:= 0;
    end;

    Close;
  end;

  sAux:= QDocs.FieldByName('tipo_impuesto_f').AsString;
  bPeninsula:= not ( UpperCase( Copy( sTipoIva, 1, 1 ) ) = 'G' );
  if sAux = 'IR' then
  begin
    sRegimenIva:= 'PEN';
    sTipoIva:= '003';
  end
  else
  if sAux = 'IC' then
  begin
    sRegimenIva:= 'CEE';
    sTipoIva:= '020';
  end
  else
  if sAux = 'IE' then
  begin
    sRegimenIva:= 'EXP';
    sTipoIva:= '030';
  end
  else
  if sAux = 'GR' then
  begin
    sRegimenIva:= 'CAN';
    sTipoIva:= '043';
  end
  else
  if sAux = 'GE' then
  begin
    sRegimenIva:= 'EXC';
    sTipoIva:= '053';
  end
  else
  begin
    sTipoIva:= '';
  end;
end;

function  TDMDContabilizacion.Divisa: string;
begin
  result:= QDocs.FieldByName('moneda_f').AsString;
end;

function  TDMDContabilizacion.CambioDivisa: string;
begin
  if QDocs.FieldByName('moneda_f').AsString = 'EUR' then
  begin
    result:= '1';
  end
  else
  begin
    with QCambio do
    begin
      ParamByName('moneda').AsString:= QDocs.FieldByName('moneda_f').AsString;
      ParamByName('fecha').AsDate:= QDocs.FieldByName('fecha_factura_f').AsDateTime;
      Open;
      if FieldByName('cambio_ce').AsFloat = 0 then
        raise Exception.Create('Falta el cambio del ' + QDocs.FieldByName('fecha_factura_f').AsString +
          ' de la moneda "' + QDocs.FieldByName('moneda_f').AsString + '".');
      result:= FormatFloat('#0.00000', 1/FieldByName('cambio_ce').AsFloat );
      QCambio.Close;
    end;
  end;
end;

function GetFechaVencimiento( const AFechaPago: TDateTime; const AIncDias, ADiaPago1, ADiaPago2: integer ): TDateTime;
var
  iDia, iMes, iAnyo: word;
  dAux: TDateTime;
begin
  dAux:= AFechaPago + AIncDias;
  if ADiaPago1 = 0 then
  begin
    result:= dAux;
  end
  else
  begin
    DecodeDate( AFechaPago, iAnyo, iMes, iDia );
    if iDia > ADiaPago1 then
    begin
      if ( ADiaPago2 = 0 ) or ( iDia > ADiaPago2 ) then
      begin
        dAux:= IncMonth( dAux );
        DecodeDate( dAux, iAnyo, iMes, iDia );
        iDia:= ADiaPago1;
      end
      else
      begin
        iDia:= ADiaPago2;
      end;
    end
    else
    begin
      iDia:= ADiaPago1;
    end;

    try
      result:= EncodeDate( iAnyo, iMes, iDia );
    except
      if iDia = 30 then
      begin
        if iMes = 2 then
        begin
          if IsLeapYear( iAnyo ) then
          begin
            result:= EncodeDate( iAnyo, iMes, 29 );
          end
          else
          begin
            result:= EncodeDate( iAnyo, iMes, 28 );
          end;
        end
        else
        begin
          result:= EncodeDate( iAnyo, iMes, 30 );
        end;
      end
      else
      begin
        if iMes = 2 then
        begin
          if IsLeapYear( iAnyo ) then
          begin
            result:= EncodeDate( iAnyo, iMes, 29 );
          end
          else
          begin
            result:= EncodeDate( iAnyo, iMes, 28 );
          end;
        end;
      end;
    end;
  end;
end;

function TDMDContabilizacion.FechaVencimiento: TDateTime;
begin
  if QDocs.FieldByName('prevision_cobro_f').Value <> NULL then
    result:= QDocs.FieldByName('prevision_cobro_f').AsDateTime
  else
    (*Se supone que estamos en la ultima salida*)
    result:= GetFechaVencimiento( QSalidas.FieldByName('fecha_sc').AsDateTime, iIncDias, iDiaPago1, iDiaPago2 );
end;


procedure TDMDContabilizacion.PutSeccion( const ASeccion, AUnidad: string; const AImporte, AGastosFac, AUnidades: Real);
var
  iPos: integer;
  bFlag: boolean;
begin
  bFlag:= false;
  iPos:= 0;
  if Length( aSecciones ) > 0 then
  begin
    while  ( iPos < Length( aSecciones ) ) and ( not bFlag ) do
    begin
      if ( aSecciones[iPos].seccion = ASeccion ) and
         ( aSecciones[iPos].unidad = AUnidad ) then
      begin
        bFlag:= True;
      end
      else
      begin
        inc( iPos );
      end;
    end;
  end;

  if bFlag then
  begin
    aSecciones[iPos].importe:= aSecciones[iPos].importe + AImporte;
    aSecciones[iPos].gastosFac:= aSecciones[iPos].gastosFac + AGastosFac;
    aSecciones[iPos].unidades:= aSecciones[iPos].unidades + AUnidades;
  end
  else
  begin
    Setlength( aSecciones, iPos + 1);
    aSecciones[iPos].seccion:= ASeccion;
    aSecciones[iPos].unidad:= AUnidad;
    aSecciones[iPos].importe:= AImporte;
    aSecciones[iPos].gastosFac:= AGastosFac;
    aSecciones[iPos].unidades:= AUnidades;
  end;
end;

procedure TDMDContabilizacion.ObtenerGastos;
begin
  rGastosPalet:= 0;
  rGastosCaja:= 0;
  rGastosKilo:= 0;
  rGastosImporte:= 0;
  rGastosTPalet:= 0;
  rGastosTCaja:= 0;
  rGastosTKilo:= 0;
  rGastosTImporte:= 0;

  with QGastos do
  begin
    ParamByName('empresa').AsString:= QSalidas.FieldByName('empresa_sc').AsString;
    ParamByName('centro').AsString:= QSalidas.FieldByName('centro_salida_sc').AsString;
    ParamByName('albaran').AsInteger:= QSalidas.FieldByName('n_albaran_sc').AsInteger;
    ParamByName('fecha').AsDate:= QSalidas.FieldByName('fecha_sc').AsDateTime;
    Open;
    while not Eof do
    begin
      if FieldByName('transito_g').AsInteger = 0 then
      begin
        if UpperCase( FieldByName('unidad_g').AsString ) = 'PALETS' then
          rGastosPalet:= FieldByName('importe_g').AsFloat
        else
        if UpperCase( FieldByName('unidad_g').AsString ) = 'KILOS' then
          rGastosKilo:= FieldByName('importe_g').AsFloat
        else
        if UpperCase( FieldByName('unidad_g').AsString ) = 'IMPORTE' then
          rGastosImporte:= FieldByName('importe_g').AsFloat
        else
        if UpperCase( FieldByName('unidad_g').AsString ) = 'CAJAS' then
          rGastosCaja:= FieldByName('importe_g').AsFloat;
      end
      else
      begin
        if UpperCase( FieldByName('unidad_g').AsString ) = 'PALETS' then
          rGastosTPalet:= FieldByName('importe_g').AsFloat
        else
        if UpperCase( FieldByName('unidad_g').AsString ) = 'KILOS' then
          rGastosTKilo:= FieldByName('importe_g').AsFloat
        else
        if UpperCase( FieldByName('unidad_g').AsString ) = 'IMPORTE' then
          rGastosTImporte:= FieldByName('importe_g').AsFloat
        else
        if UpperCase( FieldByName('unidad_g').AsString ) = 'CAJAS' then
          rGastosTCaja:= FieldByName('importe_g').AsFloat;
      end;
      Next;
    end;
    Close;
  end;
end;

procedure TDMDContabilizacion.SeccionesSalida;
var
  rGastos: real;
begin
  with QSecciones do
  begin
    ObtenerGastos;
    ParamByName('empresa').AsString:= QSalidas.FieldByName('empresa_sc').AsString;
    ParamByName('centro').AsString:= QSalidas.FieldByName('centro_salida_sc').AsString;
    ParamByName('albaran').AsInteger:= QSalidas.FieldByName('n_albaran_sc').AsInteger;
    ParamByName('fecha').AsDate:= QSalidas.FieldByName('fecha_sc').AsDateTime;
    Open;
    while not EOF do
    begin
      rGastos:= 0;
      if QSalidas.FieldByName('palets_sc').AsFloat <> 0 then
        rGastos:= rGastos + ( ( QSecciones.FieldByName('palets_s').AsFloat * rGastosPalet ) / QSalidas.FieldByName('palets_sc').AsFloat );
      if QSalidas.FieldByName('kilos_sc').AsFloat <> 0 then
        rGastos:= rGastos + ( ( QSecciones.FieldByName('kilos_s').AsFloat * rGastosKilo ) / QSalidas.FieldByName('kilos_sc').AsFloat );
      if QSalidas.FieldByName('cajas_sc').AsFloat <> 0 then
        rGastos:= rGastos + ( ( QSecciones.FieldByName('cajas_s').AsFloat * rGastosCaja ) / QSalidas.FieldByName('cajas_sc').AsFloat );
      if QSalidas.FieldByName('neto_sc').AsFloat <> 0 then
        rGastos:= rGastos + ( ( QSecciones.FieldByName('neto_s').AsFloat * rGastosImporte ) / QSalidas.FieldByName('neto_sc').AsFloat );
      if QSalidas.FieldByName('palets_tran_sc').AsFloat <> 0 then
        rGastos:= rGastos + ( ( QSecciones.FieldByName('palets_tran_s').AsFloat * rGastosTPalet ) / QSalidas.FieldByName('palets_tran_sc').AsFloat );
      if QSalidas.FieldByName('kilos_tran_sc').AsFloat <> 0 then
        rGastos:= rGastos + ( ( QSecciones.FieldByName('kilos_tran_s').AsFloat * rGastosTKilo ) / QSalidas.FieldByName('kilos_tran_sc').AsFloat );
      if QSalidas.FieldByName('cajas_tran_sc').AsFloat <> 0 then
        rGastos:= rGastos + ( ( QSecciones.FieldByName('cajas_tran_s').AsFloat * rGastosTCaja ) / QSalidas.FieldByName('cajas_tran_sc').AsFloat );
      if QSalidas.FieldByName('neto_tran_sc').AsFloat <> 0 then
        rGastos:= rGastos + ( ( QSecciones.FieldByName('neto_tran_s').AsFloat * rGastosTImporte ) / QSalidas.FieldByName('neto_tran_sc').AsFloat );
      if rGastos > 0 then
      begin
        rGastos:= SimpleRoundTo( rGastos, -2 );
      end
      else
      begin
        rGastos:= -1 * SimpleRoundTo( Abs(rGastos), -2 );
      end;

      PutSeccion( FieldByName('seccion_s').AsString, 'KG',
                  FieldByName('neto_s').AsFloat, rGastos,
                  FieldByName('kilos_s').AsFloat );
      Next;
    end;
    Close;
  end;
end;

function TDMDContabilizacion.AnyadirSecciones: Boolean;
var
  iLinea: integer;
  rImporte, rAcumImporte: Real;
begin
  Setlength( aSecciones, 0 );

  //Calcular secciones
  with QSalidas do
  begin
    while not EOf do
    begin
      SeccionesSalida;
      Next;
    end;
  end;

  //Anyadir secciones
  rAcumImporte:= 0;
  for iLinea:= 0 to length( aSecciones ) - 1 do
  begin
    rImporte:= aSecciones[iLinea].importe - SimpleRoundTo( ( aSecciones[iLinea].importe * rComision ) / 100, -2 );
    rImporte:= rImporte - SimpleRoundTo( ( rImporte * rDescuento ) / 100, -2 );
    rImporte:= rImporte + aSecciones[iLinea].gastosFac;
    rAcumImporte:= rAcumImporte + rImporte;

    //Ajustar la ultima linea
    if iLinea = length( aSecciones ) - 1 then
      rImporte:= rImporte - ( rAcumImporte - rImporteNetoFactura );
    aSecciones[iLinea].importeNeto:= rImporte;

    InsertarLineaD( iLinea );
    InsertarLineaA( iLinea );
  end;

  //Comprobar si hay descuadre
  result:= ABS( ( rAcumImporte - rImporteNetoFactura ) ) < 0.01;
end;

function  TDMDContabilizacion.FacturaAsociada: String;
begin
  if ( QDocs.FieldByName('tipo_factura_f').AsString = '381' ) and
    ( QDocs.FieldByName('concepto_f').AsString = 'A' ) then
  begin
    With QFacturaAsociada do
    begin
      ParamByName('empresa').AsString:= QDocs.fieldByName('empresa_f').AsString;
      ParamByName('abono').AsString:= QDocs.fieldByName('n_factura_f').AsString;
      ParamByName('fecha').AsString:= QDocs.fieldByName('fecha_factura_f').AsString;
      Open;
      if not IsEmpty then
      begin
        result:= NewCodigoFactura( QDocs.fieldByName('fecha_factura_f').AsString,
                                 QDocs.FieldByName('tipo_factura_f').AsString,
                                 QDocs.FieldByName('tipo_impuesto_f').AsString,
                                 FieldByName('fecha_factura_fa').AsDateTime,
                                 FieldByName('n_factura_fa').AsInteger);
      end
      else
      begin
        result:= '';
      end;
      Close;
    end;
  end
  else
  begin
    result:= '';
  end;
end;

procedure TDMDContabilizacion.ContabilizarFactura( const AFileName: string );
begin
  with QDocs do
  begin
    QSalidas.ParamByName('empresa').AsString:= QDocs.FieldByName('empresa_f').AsString;
    QSalidas.ParamByName('factura').AsInteger:= QDocs.FieldByName('n_factura_f').AsInteger;
    QSalidas.ParamByName('fecha').AsDate:= QDocs.FieldByName('fecha_factura_f').AsDateTime;
    QSalidas.Open;

    try
      sFactura:= NewCodigoFactura( FieldByName('empresa_f').AsString,
                                   FieldByName('tipo_factura_f').AsString,
                                   FieldByName('tipo_impuesto_f').AsString,
                                   FieldByName('fecha_factura_f').AsDateTime,
                                   FieldByName('n_factura_f').AsInteger);
      Tercero;
      rImporteNetoFactura:= QDocs.FieldByName('importe_neto_f').AsFloat;

      InsertarLineaT( AFileName );

      if not AnyadirSecciones then
      begin
        raise Exception.Create('Factura descuadrada.');
      end;

      InsertarLineaE;
    finally
      QSalidas.Close;
    end;

  end;
end;

procedure TDMDContabilizacion.ContaFacturaToAbonoT( const AFileName: string );
var
  i: integer;
begin
  with facturas_conta_t do
  begin
    if not Active then
      Open;

    Insert;

    for i:= 0 to Fields.Count - 1 do
    begin
      Fields[i].Value:= aux_facturas_conta_t1.Fields[i].Value;
    end;

    FieldByName('numero_fct').AsString:= sAbono;
    FieldByName('fecha_conta_fct').AsDateTime:= QDocs.FieldByName('fecha_factura_f').AsDateTime;;

    FieldByName('empresa_fct').AsString:= QDocs.FieldByName('empresa_f').AsString;
    FieldByName('fecha_factura_fct').AsDateTime:= QDocs.FieldByName('fecha_factura_f').AsDateTime;
    FieldByName('n_factura_fct').AsInteger:= QDocs.FieldByName('n_factura_f').AsInteger;

    FieldByName('factura_abono_fct').AsString:= sFactura;

    FieldByName('nombre_fichero_fct').AsString:= AFileName;

    Post;
  end;
end;

procedure TDMDContabilizacion.ContaFacturaToAbonoD;
var
  i: integer;
begin
  with facturas_conta_d do
  begin
    if not Active then
      Open;

    Insert;

    for i:= 0 to Fields.Count - 1 do
    begin
      Fields[i].Value:= aux_facturas_conta_d.Fields[i].Value;
    end;
    FieldByName('numero_fcd').AsString:= sAbono;

    Post;
  end;
end;

procedure TDMDContabilizacion.ContaFacturaToAbonoA;
var
  i: integer;
begin
  with facturas_conta_a do
  begin
    if not Active then
      Open;

    Insert;

    for i:= 0 to Fields.Count - 1 do
    begin
      Fields[i].Value:= aux_facturas_conta_a.Fields[i].Value;
    end;
    FieldByName('numero_fca').AsString:= sAbono;

    Post;
  end;
end;

procedure TDMDContabilizacion.ContaFacturaToAbonoE;
var
  i: integer;
begin
  with facturas_conta_e do
  begin
    if not Active then
      Open;

    Insert;

    for i:= 0 to Fields.Count - 1 do
    begin
      Fields[i].Value:= aux_facturas_conta_e.Fields[i].Value;
    end;
    FieldByName('numero_fce').AsString:= sAbono;

    Post;
  end;
end;

procedure TDMDContabilizacion.ContaFacturaToAbono( const AFileName: string );
begin
  with aux_facturas_conta_t1 do
  begin
    sFactura:= NewCodigoFactura( QFacturaAsociada.FieldByName('empresa_fa').AsString,
                                 '380',
                                 QDocs.FieldByName('tipo_impuesto_f').AsString,
                                 QFacturaAsociada.FieldByName('fecha_factura_fa').AsDateTime,
                                 QFacturaAsociada.FieldByName('n_factura_fa').AsInteger);


    ParamByName('empresa').AsString:=  QFacturaAsociada.FieldByName('empresa_fa').AsString;
    ParamByName('factura').AsInteger:=  QFacturaAsociada.FieldByName('n_factura_fa').AsInteger;
    ParamByName('fecha').AsDate:=  QFacturaAsociada.FieldByName('fecha_factura_fa').AsdateTime;
    Open;
    aux_facturas_conta_d.Open;
    aux_facturas_conta_a.Open;
    aux_facturas_conta_e.Open;

    ContaFacturaToAbonoT( AFileName );
    ContaFacturaToAbonoD;
    ContaFacturaToAbonoA;
    ContaFacturaToAbonoE;

    aux_facturas_conta_e.Open;
    aux_facturas_conta_a.Open;
    aux_facturas_conta_d.Open;
    Close;
  end;
end;

procedure TDMDContabilizacion.ContabilizarAbono( const AFileName: string );
begin
  DSAUX_facturas_conta.DataSet:= aux_facturas_conta_t1;
  with QDocs do
  begin
    sAbono:= NewCodigoFactura( FieldByName('empresa_f').AsString,
                                 FieldByName('tipo_factura_f').AsString,
                                 FieldByName('tipo_impuesto_f').AsString,
                                 FieldByName('fecha_factura_f').AsDateTime,
                                 FieldByName('n_factura_f').AsInteger);
  end;
  With QFacturaAsociada do
  begin
    ParamByName('empresa').AsString:= QDocs.FieldByName('empresa_f').AsString;
    ParamByName('abono').AsInteger:= QDocs.FieldByName('n_factura_f').AsInteger;
    ParamByName('fecha').AsDateTime:= QDocs.FieldByName('fecha_factura_f').AsDateTime;
    Open;
    if not IsEmpty then
    begin
      ContaFacturaToAbono( AFileName );
      Close;
    end
    else
    begin
      Close;
      Raise Exception.Create('Falta factura asociada.');
    end;
  end;
end;

procedure TDMDContabilizacion.DocContabilizado;
begin
  with mtContabilizadas do
  begin
    if not Active then
      Open;
    Insert;
    FieldByName('empresa').AsString := QDocs.FieldByName('empresa_f').AsString;
    FieldByName('factura').AsInteger := QDocs.FieldByName('n_factura_f').AsInteger;
    FieldByName('fecha').AsDateTime := QDocs.FieldByName('fecha_factura_f').AsdateTime;
    Post;
  end;
end;

procedure TDMDContabilizacion.DocNoContabilizado( const AError: string );
begin
  with mtNoContabilizadas do
  begin
    if not Active then
      Open;
    Insert;
    FieldByName('empresa').AsString := QDocs.FieldByName('empresa_f').AsString;
    FieldByName('factura').AsInteger := QDocs.FieldByName('n_factura_f').AsInteger;
    FieldByName('fecha').AsDateTime := QDocs.FieldByName('fecha_factura_f').AsdateTime;
    FieldByName('error').AsString := AError;
    Post;
  end;
end;

procedure TDMDContabilizacion.InsertarLineaT( const AFileName: string );
begin
  with facturas_conta_t do
  begin
    if not Active then
      Open;

    Insert;

    FieldByName('numero_fct').AsString:= sFactura;
    FieldByName('tipo_factura_fct').AsString:= 'FGC';
    FieldByName('planta_fct').AsString:= Planta;
    FieldByName('direccion_fct').AsString:= Direccion;
    FieldByName('fecha_conta_fct').AsDateTime:= QDocs.FieldByName('fecha_factura_f').AsDateTime;;

    FieldByName('tercero_fct').AsString:= sCtaCliente;
    FieldByName('dir_tercero_fct').AsString:= sDirTercero;
    FieldByName('divisa_fct').AsString:= Divisa;
    FieldByName('cambio_fct').AsFloat:= StrToFloat(CambioDivisa);
    FieldByName('pagador_fct').AsString:= sCtaCliente;
    FieldByName('regimen_iva_fct').AsString:= sRegimenIva;

    FieldByName('fecha_albaran_fct').AsDateTime:= QSalidas.FieldByName('fecha_sc').AsDateTime;
    FieldByName('albaran_fct').AsString:= QSalidas.FieldByName('dir_sum_sc').AsString + '-' +QSalidas.FieldByName('n_albaran_sc').AsString;
    FieldByName('factura_abono_fct').AsString:= FacturaAsociada;

    FieldByName('empresa_fct').AsString:= QDocs.FieldByName('empresa_f').AsString;
    FieldByName('fecha_factura_fct').AsDateTime:= QDocs.FieldByName('fecha_factura_f').AsDateTime;
    FieldByName('n_factura_fct').AsInteger:= QDocs.FieldByName('n_factura_f').AsInteger;

    FieldByName('nombre_fichero_fct').AsString:= AFileName;

    Post;
  end;
end;

procedure TDMDContabilizacion.InsertarLineaD( const ALinea: integer );
begin
  with facturas_conta_d do
  begin
    if not Active then
      Open;

    Insert;

    FieldByName('numero_fcd').AsString:= sFactura;
    FieldByName('linea_fcd').AsInteger:= ALinea;
    FieldByName('cuenta_general_fcd').AsString:= sCtaPGC;
    FieldByName('cuenta_analitica_fcd').AsString:= sCtaPGA;
    FieldByName('importe_ai_fcd').AsFloat:= aSecciones[ALinea].importeNeto;
    FieldByName('impuesto3_fcd').AsString:= sTipoIva;
    FieldByName('descripcion_fcd').AsString:= 'FGC ' + sCtaCliente;

    Post;
  end;
end;

procedure TDMDContabilizacion.InsertarLineaA( const ALinea: integer );
begin
  with facturas_conta_a do
  begin
    if not Active then
      Open;

    Insert;

    FieldByName('numero_fca').AsString:= sFactura;
    FieldByName('linea_fca').AsInteger:= ALinea;
    FieldByName('linea_seccion_fca').AsInteger:= 1;
    FieldByName('cuenta_analitica_fca').AsString:= sCtaPGA;
    FieldByName('seccion_fca').AsString:= aSecciones[ALinea].seccion;
    FieldByName('importe_fca').AsFloat:= aSecciones[ALinea].importeNeto;
    FieldByName('unidad_fca').AsString:= aSecciones[ALinea].unidad;
    FieldByName('cantidad_fca').AsFloat:= aSecciones[ALinea].unidades;

    Post;
  end;
end;

procedure TDMDContabilizacion.InsertarLineaE;
begin
  with facturas_conta_e do
  begin
    if not Active then
      Open;

    Insert;

    FieldByName('numero_fce').AsString:= sFactura;
    FieldByName('linea_fce').AsInteger:= 1;
    FieldByName('vencimiento_fce').AsDateTime:= FechaVencimiento;
    FieldByName('modo_pago_fce').AsString:= sFormaPago;
    FieldByName('importe_divisa_fce').AsFloat:= QDocs.FieldByName('importe_total_f').AsFloat;
    FieldByName('direccion_fce').AsString:= '001';

    Post;
  end;
end;


procedure TDMDContabilizacion.AddFacturaFicheroTexto( var ATexto: TStringList );
var
  sCad: string;
begin
  sCad:= 'T';
  sCad:= sCad + ';FGC;' + aux_facturas_conta_t2.FieldByName('numero_fct').AsString;
  sCad:= sCad + ';' + aux_facturas_conta_t2.FieldByName('planta_fct').AsString;
  sCad:= sCad + ';' + aux_facturas_conta_t2.FieldByName('direccion_fct').AsString;
  sCad:= sCad + ';' + FechaAdonix( aux_facturas_conta_t2.FieldByName('fecha_conta_fct').AsDateTime );
  sCad:= sCad + ';' + aux_facturas_conta_t2.FieldByName('tercero_fct').AsString;
  sCad:= sCad + ';' + aux_facturas_conta_t2.FieldByName('dir_tercero_fct').AsString;
  sCad:= sCad + ';' + aux_facturas_conta_t2.FieldByName('divisa_fct').AsString;
  sCad:= sCad + ';' + aux_facturas_conta_t2.FieldByName('cambio_fct').AsString;
  sCad:= sCad + ';' + aux_facturas_conta_t2.FieldByName('pagador_fct').AsString;
  sCad:= sCad + ';' + aux_facturas_conta_t2.FieldByName('regimen_iva_fct').AsString;
  sCad:= sCad + ';;' + FechaAdonix( aux_facturas_conta_t2.FieldByName('fecha_albaran_fct').AsDateTime );
  sCad:= sCad + ';' + aux_facturas_conta_t2.FieldByName('albaran_fct').AsString;
  sCad:= sCad + ';' + aux_facturas_conta_t2.FieldByName('factura_abono_fct').AsString;
  ATexto.Add( sCad );

  while not aux_facturas_conta_d.Eof do
  begin
    sCad:= 'D;' + intToStr( aux_facturas_conta_d.FieldByName('linea_fcd').AsInteger );
    sCad:= sCad + ';' + aux_facturas_conta_d.FieldByName('cuenta_general_fcd').AsString;
    sCad:= sCad + ';' + aux_facturas_conta_d.FieldByName('cuenta_analitica_fcd').AsString;
    sCad:= sCad + ';' + FormatFloat( '#0.00', aux_facturas_conta_d.FieldByName('importe_ai_fcd').AsFloat );
    sCad:= sCad + ';' + aux_facturas_conta_d.FieldByName('impuesto3_fcd').AsString;
    sCad:= sCad + ';' + aux_facturas_conta_d.FieldByName('descripcion_fcd').AsString;
    ATexto.Add( sCad );

    while not aux_facturas_conta_a.Eof do
    begin
      sCad:= 'A;' + intToStr( aux_facturas_conta_a.FieldByName('linea_fca').AsInteger );
      sCad:= sCad + ';' + intToStr( aux_facturas_conta_a.FieldByName('linea_seccion_fca').AsInteger );
      sCad:= sCad + ';' + aux_facturas_conta_a.FieldByName('cuenta_analitica_fca').AsString;
      sCad:= sCad + ';' + aux_facturas_conta_a.FieldByName('seccion_fca').AsString;
      sCad:= sCad + ';;;';
      sCad:= sCad + ';' + FormatFloat( '#0.00', aux_facturas_conta_a.FieldByName('importe_fca').AsFloat);
      sCad:= sCad + ';' + aux_facturas_conta_a.FieldByName('unidad_fca').AsString;
      sCad:= sCad + ';' + FormatFloat( '#0.00', aux_facturas_conta_a.FieldByName('cantidad_fca').AsFloat );
      ATexto.Add( sCad );
      aux_facturas_conta_a.Next;
    end;

    aux_facturas_conta_d.Next;
  end;

  while not aux_facturas_conta_e.Eof do
  begin
    sCad:= 'E;' + aux_facturas_conta_e.FieldByName('linea_fce').AsString;
    sCad:= sCad + ';' + FechaAdonix( aux_facturas_conta_e.FieldByName('vencimiento_fce').AsDateTime );
    sCad:= sCad + ';' + aux_facturas_conta_e.FieldByName('modo_pago_fce').AsString;
    sCad:= sCad + ';' + FormatFloat( '#0.00', aux_facturas_conta_e.FieldByName('importe_divisa_fce').AsFloat );
    sCad:= sCad + ';' + aux_facturas_conta_e.FieldByName('direccion_fce').AsString;
    ATexto.Add( sCad );
    aux_facturas_conta_e.Next;
  end;
end;

procedure TDMDContabilizacion.FicheroTexto( const AFileName: string; var ATexto: TStringList );
begin
  DSAUX_facturas_conta.DataSet:= aux_facturas_conta_t2;
  aux_facturas_conta_t2.ParamByName('fichero').AsString:= AFileName;
  aux_facturas_conta_t2.Open;
  aux_facturas_conta_d.Open;
  aux_facturas_conta_a.Open;
  aux_facturas_conta_e.Open;
  while not aux_facturas_conta_t2.Eof do
  begin
    AddFacturaFicheroTexto( ATexto );
    aux_facturas_conta_t2.Next;
  end;
  aux_facturas_conta_t2.Close;
end;

end.
