unit CFDClave;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, nbLabels;

type
  TDFDClave = class(TForm)
    lblUsuario: TnbLabel;
    lblClave: TnbLabel;
    eUsuario: TEdit;
    eClave: TEdit;
    btnAceptar: TButton;
    btnCancelar: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnAceptarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    sUsuario, sClave: string;
    bAceptar: boolean;
  end;

  function Ejecutar( var AUsuario, AClave: string ): boolean;


implementation

{$R *.dfm}

var
  DFDClave: TDFDClave;

function Ejecutar( var AUsuario, AClave: string ): boolean;
begin
  DFDClave:= TDFDClave.Create( nil );
  DFDClave.ShowModal;
  result:= DFDClave.bAceptar;
  if result then
  begin
    AUsuario:= DFDClave.sUsuario;
    AClave:= DFDClave.sClave;
  end;
  FreeAndNil( DFDClave );
end;

procedure TDFDClave.FormKeyDown(Sender: TObject; var Key: Word;
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
        btnAceptar.Click;
      end;
    vk_escape:
      begin
        Key := 0;
        btnCancelar.Click;
      end;
  end;
end;

procedure TDFDClave.btnCancelarClick(Sender: TObject);
begin
  bAceptar:= False;
  Close;
end;

procedure TDFDClave.btnAceptarClick(Sender: TObject);
begin
  bAceptar:= True;
  sUsuario:= eUsuario.Text;
  sClave:= eClave.Text;
  Close;
end;

end.

