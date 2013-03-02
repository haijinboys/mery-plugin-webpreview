object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'MainForm'
  ClientHeight = 338
  ClientWidth = 651
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object WebBrowser: TWebBrowser
    Left = 0
    Top = 0
    Width = 651
    Height = 338
    Align = alClient
    PopupMenu = PopupMenu
    TabOrder = 0
    OnCommandStateChange = WebBrowserCommandStateChange
    OnNavigateComplete2 = WebBrowserNavigateComplete2
    ExplicitLeft = 304
    ExplicitTop = 160
    ExplicitWidth = 300
    ExplicitHeight = 150
    ControlData = {
      4C00000048430000EF2200000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object PopupMenu: TPopupMenu
    AutoHotkeys = maManual
    OnPopup = PopupMenuPopup
    Left = 8
    Top = 8
    object BackMenuItem: TMenuItem
      Caption = #21069#12395#25147#12427'(&B)'
      OnClick = BackMenuItemClick
    end
    object ForwardMenuItem: TMenuItem
      Caption = #27425#12395#36914#12416'(&F)'
      OnClick = ForwardMenuItemClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object RefreshMenuItem: TMenuItem
      Caption = #26368#26032#12398#24773#22577#12395#26356#26032'(&R)'
      OnClick = RefreshMenuItemClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object PropMenuItem: TMenuItem
      Caption = #12503#12525#12497#12486#12451'(&P)'
      OnClick = PropMenuItemClick
    end
  end
end
