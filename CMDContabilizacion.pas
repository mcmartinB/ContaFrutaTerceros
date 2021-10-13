unit CMDContabilizacion;

interface

uses
  SysUtils, Classes, DB, DBTables, kbmMemTable, Dialogs;

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
    QFacturas: TQuery;
    QNumFacturas: TQuery;
    QSalidas: TQuery;
    QGastos: TQuery;
    Database: TDatabase;
    QImporteFacturas: TQuery;
    QDescuentoCliente: TQuery;
    QAux: TQuery;
    QTotalesSalidas: TQuery;
    qryProductoSal: TQuery;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    aSecciones: array of TRSeccion;
    rImporteNetoFactura, rDescuento, rComision: real;
    rGastosPalet, rGastosCaja, rGastosKilo, rGastosImporte: real;
    rGastosTPalet, rGastosTCaja, rGastosTKilo, rGastosTImporte: real;
    rNew, rOld: real;
    sFileName: string;

    rImporte_total_f, rImporte_neto_f: Real;
    sCodFactura, sMoneda, sCambio: string;

    function  FechaAdonix( const AFecha: TDateTime ): string;
    function  GetCambio: string;
    function  GetCuenta: string;
    function  GetImporte: string;
    function  GetKilos: string;
    function  GetSeccionOrigen: string;
    function  GetSeccionDestino: string;

    function  ObtenerGastos: real;

    procedure Lineas;

    procedure ImporteFacturaAutomatica;

    procedure ContabilizarFactura;

    (*
    procedure NewSeccion( const ASeccion: string; const AImporte, AKilos: Real );
    procedure OldSeccion( const ASeccion: string; const AImporte, AKilos: Real );
    procedure ActualizarSecciones;
    *)
  public
    { Public declarations }
    slFichero, slListado: TStringList;

    procedure PreparaQuerys( const AEmpresa: string );

    function  SeleccionarFacturas( const AEmpresa: string; const AFechaIni, AFechaFin: TDateTime ): integer;
    function  ContabilizaFacturas( const AFilename: string; var sMsg: string ): boolean;

    function  FicheroTexto( const AFileName: string ): boolean;

  end;

var
  DMDContabilizacion: TDMDContabilizacion;

implementation

{$R *.dfm}

uses Variants, bMath, bTextUtils;

(**)
procedure TDMDContabilizacion.PreparaQuerys( const AEmpresa: string );
begin
  with QNumFacturas do
  begin
    Close;
    if Prepared then
      UnPrepare;
    SQL.Clear;
    SQL.Add(' select count( distinct n_factura_sc ) ');
    SQL.Add(' from frf_salidas_l ');
    SQL.Add('      join frf_salidas_c on empresa_sl = empresa_sc and centro_salida_sl = centro_salida_sc ');
    SQL.Add('                         and n_albaran_sc = n_albaran_sl and fecha_sl = fecha_sc ');
    SQL.Add(' where empresa_sl <> emp_procedencia_sl ');
    SQL.Add(' and empresa_sc = :empresa ');
    SQL.Add(' and n_factura_sc is not null ');
    SQL.Add(' and fecha_sl between :fechaini and :fechafin ');
    SQL.Add(' and producto_sl <> "GRS" ');
    Prepare;
  end;

  with QFacturas do
  begin
    Close;
    if Prepared then
      UnPrepare;
    SQL.Clear;
    SQL.Add(' select empresa_fac_sc, serie_fac_sc, fecha_factura_sc,  n_factura_sc, cliente_fac_sc, moneda_sc ');
    SQL.Add(' from frf_salidas_l ');
    SQL.Add('      join frf_salidas_c on empresa_sl = empresa_sc and centro_salida_sl = centro_salida_sc ');
    SQL.Add('                         and n_albaran_sc = n_albaran_sl and fecha_sl = fecha_sc ');
    SQL.Add(' where empresa_sl <> emp_procedencia_sl ');
    SQL.Add(' and empresa_sc = :empresa ');
    SQL.Add(' and fecha_sl between :fechaini and :fechafin ');
    SQL.Add(' and n_factura_sc is not null ');
    SQL.Add(' and producto_sl <> "GRS" ');
    SQL.Add(' group by empresa_fac_sc, serie_fac_sc, fecha_factura_sc,  n_factura_sc, cliente_fac_sc, moneda_sc ');
    SQL.Add(' order by empresa_fac_sc, serie_fac_sc, fecha_factura_sc,  n_factura_sc ');
    Prepare;
  end;

  with QImporteFacturas do
  begin
    SQL.Clear;
    SQL.Add(' select cod_factura_fc, importe_neto_fc, importe_neto_euros_fc, importe_total_fc, moneda_fc ');
    SQL.Add('  from tfacturas_cab ');
    SQL.Add(' where cod_empresa_fac_fc = :empresa ');
    SQL.Add(' and fecha_factura_fc = :fecha ');
    SQL.Add(' and n_factura_fc = :factura ');
    SQL.Add(' and cod_serie_fac_fc = :serie ');
    Prepare;
  end;

  with QSalidas do
  begin
    SQL.Clear;
    SQL.Add('  select sc.empresa_sc, sc.centro_salida_sc, sc.n_albaran_sc, sc.fecha_sc,  ');
    SQL.Add('         sc.dir_sum_sc, se1.sec_contable_rl seccion_ini, se2.sec_contable_rl seccion_fin, cli.cta_ingresos_pga_c,    ');

    SQL.Add('      sum(nvl(n_palets_sl,0)) palets_s,  ');
    SQL.Add('      sum(nvl(cajas_sl,0)) cajas_s, ');
    SQL.Add('      sum(nvl(kilos_sl,0)) kilos_s,  ');
    SQL.Add('      sum(nvl(importe_neto_sl,0)) neto_s, ');

    SQL.Add('      sum(case when ref_transitos_sl is not null then nvl(n_palets_sl,0) else 0 end) palets_tran_s,  ');
    SQL.Add('      sum(case when ref_transitos_sl is not null then nvl(cajas_sl,0) else 0 end) cajas_tran_s, ');
    SQL.Add('      sum(case when ref_transitos_sl is not null then nvl(kilos_sl,0) else 0 end) kilos_tran_s, ');
    SQL.Add('      sum(case when ref_transitos_sl is not null then nvl(importe_neto_sl,0) else 0 end) neto_tran_s, ');

    SQL.Add('      sum(nvl(cajas_st,0)) cajas_st,                                                                  ');
    SQL.Add('      sum(nvl(kilos_st,0)) kilos_st,                                                                  ');
    SQL.Add('      sum(nvl(importe_neto_st,0)) neto_st                                                             ');

    SQL.Add('  from frf_salidas_c sc ');
    SQL.Add('       join frf_salidas_l sl on sl.empresa_sl = sc.empresa_sc  and sl.centro_salida_sl = sc.centro_salida_sc  ');
    SQL.Add('            and sl.n_albaran_sl = sc.n_albaran_sc and sl.fecha_sl = sc.fecha_sc  ');
    SQL.Add('       join rsecciones_linea se1 on se1.empresa_rl = sl.empresa_sl ');
    SQL.Add('            and se1.producto_rl = sl.producto_sl and se1.centro_rl = sl.centro_salida_sl  ');
    SQL.Add('       join rsecciones_linea se2 on se2.empresa_rl = sl.emp_procedencia_sl ');
    SQL.Add('            and se2.producto_rl = sl.producto_sl and se2.centro_rl = sl.centro_salida_sl  ');
    SQL.Add('       join frf_clientes cli on sc.cliente_sal_sc = cli.cliente_c ');
    SQL.Add('       join frf_salidas_terceros st on st.empresa_st = sl.empresa_sl  and st.centro_salida_st = sl.centro_salida_sl                  ');
    SQL.Add('            and st.n_albaran_st = sl.n_albaran_sl and st.fecha_st = sl.fecha_sl and st.id_linea_albaran_st = sl.id_linea_albaran_sl  ');


    SQL.Add('  where sc.empresa_fac_sc = :empresa ');
    SQL.Add('  and sc.n_factura_sc = :factura  ');
    SQL.Add('  and sc.fecha_factura_sc = :fecha ');
    SQL.Add('  and sc.serie_fac_sc = :serie ');
    SQL.Add('  and sl.emp_procedencia_sl <> sc.empresa_sc  ');

    SQL.Add('  group by sc.empresa_sc, sc.centro_salida_sc, sc.n_albaran_sc, sc.fecha_sc,  ');
    SQL.Add('           sc.dir_sum_sc , se1.sec_contable_rl, se2.sec_contable_rl, cli.cta_ingresos_pga_c  ');
    (*
    if AEmpresa = '050' then
      SQL.Add(' and centro_sc = centro_origen_sl ')
    else
      SQL.Add(' and centro_sc = centro_salida_sl ');
    *)
    Prepare;
  end;


  with qryProductoSal do
  begin
    SQL.Clear;
    SQL.Add('  select producto_sl ');
    SQL.Add('  from frf_salidas_c sc ');
    SQL.Add('       join frf_salidas_l sl on sl.empresa_sl = sc.empresa_sc  and sl.centro_salida_sl = sc.centro_salida_sc ');
    SQL.Add('            and sl.n_albaran_sl = sc.n_albaran_sc and sl.fecha_sl = sc.fecha_sc ');
    SQL.Add('  where sc.empresa_fac_sc = :empresa ');
    SQL.Add('  and sc.n_factura_sc = :factura  ');
    SQL.Add('  and sc.fecha_factura_sc = :fecha ');
    SQL.Add('  and sc.serie_fac_sc = :serie ');
    SQL.Add('  and sl.emp_procedencia_sl <> sc.empresa_sc ');
  end;

  with QTotalesSalidas do
  begin
    SQL.Clear;
    SQL.Add(' select ');
    SQL.Add('      sum(nvl(n_palets_sl,0)) palets_sc, ');
    SQL.Add('      sum(nvl(cajas_sl,0)) cajas_sc, ');
    SQL.Add('      sum(nvl(kilos_sl,0)) kilos_sc, ');
    SQL.Add('      sum(nvl(importe_neto_sl,0)) neto_sc, ');

    SQL.Add('      sum(case when ref_transitos_sl is not null then nvl(n_palets_sl,0) else 0 end) palets_tran_sc, ');
    SQL.Add('      sum(case when ref_transitos_sl is not null then nvl(cajas_sl,0) else 0 end) cajas_tran_sc, ');
    SQL.Add('      sum(case when ref_transitos_sl is not null then nvl(kilos_sl,0) else 0 end) kilos_tran_sc, ');
    SQL.Add('      sum(case when ref_transitos_sl is not null then nvl(importe_neto_sl,0) else 0 end) neto_tran_sc ');

    SQL.Add('  from frf_salidas_l ');

    SQL.Add('  where empresa_sl = :empresa ');
    SQL.Add('  and centro_salida_sl = :centro ');
    SQL.Add('  and n_albaran_sl = :albaran ');
    SQL.Add('  and fecha_sl = :fecha ');
  end;

  with QDescuentoCliente do
  begin
    SQL.Clear;
    SQL.Add(' select GetComisionCliente( :empresa,:cliente,:fecha) comision, GetDescuentoCliente( :empresa,:cliente,:fecha,1) descuento ');
    SQL.Add(' from frf_clientes ');
    SQL.Add(' where cliente_c = :cliente ');
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

end;


procedure TDMDContabilizacion.DataModuleDestroy(Sender: TObject);
begin
  if Database.Connected then
  begin

    QNumFacturas.Close;
    if QNumFacturas.Prepared then
      QNumFacturas.UnPrepare;
    QFacturas.Close;
    if QFacturas.Prepared then
      QFacturas.UnPrepare;
    QImporteFacturas.Close;
    if QImporteFacturas.Prepared then
      QImporteFacturas.UnPrepare;

    QSalidas.Close;
    if QSalidas.Prepared then
      QSalidas.UnPrepare;
    QDescuentoCliente.Close;
    if QDescuentoCliente.Prepared then
      QDescuentoCliente.UnPrepare;

    QGastos.Close;
    if QGastos.Prepared then
      QGastos.UnPrepare;

    freeandnil( slFichero );
    freeandnil( slListado );
  end;
end;

procedure TDMDContabilizacion.DataModuleCreate(Sender: TObject);
begin
  slFichero:= TStringList.create;
  slListado:= TStringList.create;
end;

function TDMDContabilizacion.SeleccionarFacturas( const AEmpresa: string;
  const AFechaIni, AFechaFin: TDateTime ): integer;
begin
  with QNumFacturas do
  begin
    ParamByName('empresa').AsString:= AEmpresa;
    ParamByName('fechaini').AsDate:= AFechaIni;
    ParamByName('fechafin').AsDate:= AFechaFin;
    Open;
    result:= Fields[0].AsInteger;
    Close;

    if result = 0 then
      Exit;
  end;

  with QFacturas do
  begin
    Close;
    ParamByName('empresa').AsString:= AEmpresa;
    ParamByName('fechaini').AsDate:= AFechaIni;
    ParamByName('fechafin').AsDate:= AFechaFin;
    Open;
  end;
end;

function  TDMDContabilizacion.FechaAdonix( const AFecha: TDateTime ): string;
begin
  result:= StringReplace( DateToStr( AFecha ), '/', '', [rfReplaceAll, rfIgnoreCase] );
end;

(**)
function  TDMDContabilizacion.ObtenerGastos: real;
begin
  Result:= 0;
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
    if not IsEmpty then
    begin
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

      QTotalesSalidas.ParamByName('empresa').AsString:= QSalidas.FieldByName('empresa_sc').AsString;
      QTotalesSalidas.ParamByName('centro').AsString:= QSalidas.FieldByName('centro_salida_sc').AsString;
      QTotalesSalidas.ParamByName('albaran').AsInteger:= QSalidas.FieldByName('n_albaran_sc').AsInteger;
      QTotalesSalidas.ParamByName('fecha').AsDate:= QSalidas.FieldByName('fecha_sc').AsDateTime;
      QTotalesSalidas.Open;

      if ( QSalidas.FieldByName('palets_s').AsFloat <> 0 ) and ( rGastosPalet <> 0 ) then
        Result:= Result + ( ( QSalidas.FieldByName('palets_s').AsFloat * rGastosPalet ) / QTotalesSalidas.FieldByName('palets_sc').AsFloat );
      if ( QSalidas.FieldByName('kilos_st').AsFloat <> 0 ) and ( rGastosKilo <> 0 ) then
        Result:= Result + ( ( QSalidas.FieldByName('kilos_st').AsFloat * rGastosKilo ) / QTotalesSalidas.FieldByName('kilos_sc').AsFloat );
      if ( QSalidas.FieldByName('cajas_st').AsFloat <> 0 ) and ( rGastosCaja <> 0 ) then
        Result:= Result + ( ( QSalidas.FieldByName('cajas_st').AsFloat * rGastosCaja ) / QTotalesSalidas.FieldByName('cajas_sc').AsFloat );
      if ( QSalidas.FieldByName('neto_st').AsFloat <> 0 ) and ( rGastosImporte <> 0 ) then
        Result:= Result + ( ( QSalidas.FieldByName('neto_st').AsFloat * rGastosImporte ) / QTotalesSalidas.FieldByName('neto_sc').AsFloat );
      if ( QSalidas.FieldByName('palets_tran_s').AsFloat <> 0 ) and ( rGastosTPalet <> 0 ) then
        Result:= Result + ( ( QSalidas.FieldByName('palets_tran_s').AsFloat * rGastosTPalet ) / QTotalesSalidas.FieldByName('palets_tran_sc').AsFloat );
      if ( QSalidas.FieldByName('kilos_tran_s').AsFloat <> 0 ) and ( rGastosTKilo <> 0 ) then
        Result:= Result + ( ( QSalidas.FieldByName('kilos_tran_s').AsFloat * rGastosTKilo ) / QTotalesSalidas.FieldByName('kilos_tran_sc').AsFloat );
      if ( QSalidas.FieldByName('cajas_tran_s').AsFloat <> 0 ) and ( rGastosTCaja <> 0 ) then
        Result:= Result + ( ( QSalidas.FieldByName('cajas_tran_s').AsFloat * rGastosTCaja ) / QTotalesSalidas.FieldByName('cajas_tran_sc').AsFloat );
      if ( QSalidas.FieldByName('neto_tran_s').AsFloat <> 0 ) and ( rGastosTImporte <> 0 ) then
        Result:= Result + ( ( QSalidas.FieldByName('neto_tran_s').AsFloat * rGastosTImporte ) / QTotalesSalidas.FieldByName('neto_tran_sc').AsFloat );

      QTotalesSalidas.Close;
      if Result > 0 then
      begin
        Result:= bRoundTo( Result, -2 );
      end
      else
      begin
        Result:= -1 * bRoundTo( Abs(Result), -2 );
      end;
    end;
   Close;
  end;
end;

(**)
(*Sacar de la contabilizacion*)
procedure TDMDContabilizacion.ImporteFacturaAutomatica;
begin
  with QImporteFacturas do
  begin
    ParamByName('empresa').AsString:= QFacturas.FieldByName('empresa_fac_sc').AsString;
    ParamByName('factura').AsInteger:= QFacturas.FieldByName('n_factura_sc').AsInteger;
    ParamByName('fecha').AsDate:= QFacturas.FieldByName('fecha_factura_sc').AsDateTime;
    ParamByName('serie').AsString:= QFacturas.FieldByName('serie_fac_sc').AsString;
    Open;
    //cod_factura_fc, importe_neto_fc, importe_total_fc, moneda_fc
    rImporte_total_f:= FieldByName('importe_total_fc').AsFloat;
    rImporte_neto_f:= FieldByName('importe_neto_fc').AsFloat;
    sCodFactura:= FieldByName('cod_factura_fc').AsString;
    sMoneda:= FieldByName('moneda_fc').AsString;
    sCambio:= GetCambio;
    Close;
  end;
  with QDescuentoCliente do
  begin
    ParamByName('empresa').AsString:= QFacturas.FieldByName('empresa_fac_sc').AsString;
    ParamByName('cliente').AsString:= QFacturas.FieldByName('cliente_fac_sc').AsString;
    ParamByName('fecha').AsDateTime:= QFacturas.FieldByName('fecha_factura_sc').AsDateTime;
    Open;
    rDescuento:= FieldByName('descuento').AsFloat;
    rComision:= FieldByName('comision').AsFloat;
    Close;
  end;
end;

function TDMDContabilizacion.GetCambio: string;
var
  rAux: Real;
begin
  if ( QImporteFacturas.FieldByName('importe_neto_fc').AsFloat <> 0 ) and
     ( QImporteFacturas.FieldByName('importe_neto_fc').AsFloat <> QImporteFacturas.FieldByName('importe_neto_euros_fc').AsFloat ) then
  begin
    rAux:= bRoundTo( QImporteFacturas.FieldByName('importe_neto_euros_fc').AsFloat / QImporteFacturas.FieldByName('importe_neto_fc').AsFloat, 5 );
  end
  else
  begin
    rAux:= 1
  end;
  Result:= FormatFloat('0.00000', rAux);
end;

procedure TDMDContabilizacion.ContabilizarFactura;
var
  sProducto: string;
begin
  ImporteFacturaAutomatica;
  QSalidas.ParamByName('empresa').AsString:= QFacturas.FieldByName('empresa_fac_sc').AsString;
  QSalidas.ParamByName('factura').AsInteger:= QFacturas.FieldByName('n_factura_sc').AsInteger;
  QSalidas.ParamByName('fecha').AsDate:= QFacturas.FieldByName('fecha_factura_sc').AsDateTime;
  QSalidas.ParamByName('serie').AsString:= QFacturas.FieldByName('serie_fac_sc').AsString;
  QSalidas.Open;

  if not  QSalidas.Isempty then
  begin
    while not QSalidas.Eof do
    begin
      Lineas;
      QSalidas.Next;
    end;
  end
  else
  begin
    QSalidas.Close;
    qryProductoSal.ParamByName('empresa').AsString:= QFacturas.FieldByName('empresa_fac_sc').AsString;
    qryProductoSal.ParamByName('factura').AsInteger:= QFacturas.FieldByName('n_factura_sc').AsInteger;
    qryProductoSal.ParamByName('fecha').AsDate:= QFacturas.FieldByName('fecha_factura_sc').AsDateTime;
    qryProductoSal.ParamByName('serie').AsString:= QFacturas.FieldByName('serie_fac_sc').AsString;
    qryProductoSal.Open;
    sProducto:= qryProductoSal.FieldByname('producto_sl').AsString;
    qryProductoSal.Close;
    raise Exception.create( 'Falta sección contable para compras tercero del producto ' + sProducto + '.' );
  end;
  QSalidas.Close;
end;

function TDMDContabilizacion.GetImporte: string;
var
  rImporte: Real;
begin
  rImporte:= QSalidas.FieldByName('neto_st').AsFloat;
  if rComision > 0 then
    rImporte:= rImporte - bRoundTo( ( rImporte * rComision ) / 100, -2 );
  if rDescuento > 0 then
    rImporte:= rImporte - bRoundTo( ( rImporte * rDescuento ) / 100, -2 );
  rImporte:= rImporte + ObtenerGastos;
  Result:=  FormatFloat('0.##', Abs( rImporte ) )
end;

function TDMDContabilizacion.GetKilos: string;
begin
  Result:=  FormatFloat('0.##', ABS( QSalidas.FieldByName('kilos_st').AsFloat ) )
end;

function TDMDContabilizacion.GetCuenta: string;
begin
  result:= QSalidas.FieldByName('cta_ingresos_pga_c').AsString;
end;

function TDMDContabilizacion.GetSeccionOrigen: string;
begin
  result:= QSalidas.FieldByName('seccion_ini').AsString;
end;

function TDMDContabilizacion.GetSeccionDestino: string;
begin
  result:= QSalidas.FieldByName('seccion_fin').AsString;
end;


procedure TDMDContabilizacion.Lineas;
var
  sImporte, sKilos: string;
begin
  sImporte:= GetImporte;
  sKilos:= GetKilos;
  slFichero.Add( '"G";' +
                   '"ODA";'+
                   '"";' +
                   '"' + QFacturas.FieldByName('empresa_fac_sc').AsString + '";' +                                       //PLANTA
                   '"' + FechaAdonix(QFacturas.FieldByName('fecha_factura_sc').AsDateTime )  + '";' +
                   '"";' +
                   '"' + sCodFactura + '";' +
                   '"";' +
                   '"' + FechaAdonix( QFacturas.FieldByName('fecha_factura_sc').AsDateTime ) + '";' +
                   '"' + sMoneda + '";' +
                   '"' + sCambio  + '"');

  //"D";"1";"2";;"TRASPASO NO SOCIOS";"1";"17,2"
  slFichero.Add( '"D";' +
                 '"1";' +
                 '"2";' +
                 '"' + GetCuenta + '";'+
                 '"TRASPASO NO SOCIOS";' +
                 '"1";'+
                 '"' + sImporte  + '"' );

  //"A";"1";"1";"AX1";"050-CP0001";"";"";"";"";"";"";"";"";"KG";"8";"17,2"
  slFichero.Add( '"A";' +
                 '"1";' +
                 '"1";' +
                 '"AX1";' +
                 '"' + GetSeccionOrigen + '";' +
                 '"";"";"";"";"";"";"";"";' +
                 '"KG";' +
                 '"' + sKilos + '";' +
                 '"' + sImporte + '"' );

  //"D";"2";"2";"70110001";"TRASPASO NO SOCIOS";"-1";"17,2"
  slFichero.Add( '"D";' +
                 '"2";' +
                 '"2";' +
                 '"' + GetCuenta + '";'+
                 '"TRASPASO NO SOCIOS";' +
                 '"-1";'+
                 '"' + sImporte  + '"' );

  //"A";"2";"1";"AX1";"050-CP0002";"";"";"";"";"";"";"";"";"KG";"8";"17,2"
  slFichero.Add( '"A";' +
                 '"2";' +
                 '"1";' +
                 '"AX1";' +
                 '"' + GetSeccionDestino + '";' +
                 '"";"";"";"";"";"";"";"";' +
                 '"KG";' +
                 '"' + sKilos + '";' +
                 '"' + sImporte + '"' );
  slListado.Add('"' + QFacturas.FieldByName('empresa_fac_sc').AsString + '";' +
                '"' + QFacturas.FieldByName('serie_fac_sc').AsString + '";' +
                '"' + QFacturas.FieldByName('fecha_factura_sc').AsString + '";' +
                '"' + QFacturas.FieldByName('n_factura_sc').AsString + '";' +
                '"' + GetCuenta + '";'+
                '"' + GetSeccionOrigen + '";' +
                '"' + GetSeccionDestino + '";' +
                '"' + sKilos + '";' +
                '"' + sImporte + '"' );
end;

function TDMDContabilizacion.FicheroTexto( const AFileName: string ): boolean;
begin
  if slFichero.Count > 0 then
  begin
    slFichero.SaveToFile( AFileName );
    result:= True;
  end
  else
  begin
    result:= False;
  end;
end;

function TDMDContabilizacion.ContabilizaFacturas( const AFilename: string; var sMsg: string ): boolean;
begin
  sFileName:= AFileName;
  slFichero.Clear;
  slListado.Clear;
  sMsg:= '';
  //Contabilizar facturas
  QFacturas.First;
  while not QFacturas.Eof do
  begin
    try
      If not DMDContabilizacion.Database.InTransaction then
      begin
        Database.StartTransaction;
        ContabilizarFactura;
        Database.Commit;
      end;
    except
      on e: exception do
      begin
        if sMsg = '' then
        begin
          sMsg:= e.Message;
        end;

        Database.Rollback;
      end;
      //Seguir con el resto de facturas
    end;
    QFacturas.Next;
  end;
  QFacturas.Close;

  result:= sMsg = '';
  if sMsg <> '' then
  begin
    ShowMessage('ERROR AL CONTABILIZAR -> ' + #13 + #10 + sMsg );
  end;
end;

end.
