program RecontabilizarFacturas;

uses
  Forms,
  CMDContabilizacion in 'CMDContabilizacion.pas' {DMDContabilizacion: TDataModule},
  CFDContabilizacion in 'CFDContabilizacion.pas' {DFDContabilizacion},
  CFDClave in 'CFDClave.pas' {DFDClave};

{$R *.res}

begin
  Application.Initialize;
  try
    Application.CreateForm(TDFDContabilizacion, DFDContabilizacion);
  finally
    if not DMDContabilizacion.Database.Connected then
      Application.Terminate
    else
      Application.Run;
  end;
end.
