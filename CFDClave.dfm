object DFDClave: TDFDClave
  Left = 810
  Top = 417
  ActiveControl = eUsuario
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = '   IDENTIFICACI'#211'N USUARIO'
  ClientHeight = 140
  ClientWidth = 284
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object lblUsuario: TnbLabel
    Left = 24
    Top = 24
    Width = 100
    Height = 21
    Caption = 'Usuario'
    About = 'NB 0.1/20020725'
  end
  object lblClave: TnbLabel
    Left = 24
    Top = 48
    Width = 100
    Height = 21
    Caption = 'Clave'
    About = 'NB 0.1/20020725'
  end
  object eUsuario: TEdit
    Left = 128
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object eClave: TEdit
    Left = 128
    Top = 48
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object btnAceptar: TButton
    Left = 96
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Aceptar (F1)'
    TabOrder = 2
    TabStop = False
    OnClick = btnAceptarClick
  end
  object btnCancelar: TButton
    Left = 174
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Cancelar (Esc)'
    TabOrder = 3
    TabStop = False
    OnClick = btnCancelarClick
  end
end
