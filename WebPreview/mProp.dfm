object PropForm: TPropForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Web'#12503#12524#12499#12517#12540
  ClientHeight = 297
  ClientWidth = 281
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
  object BarPosLabel: TLabel
    Left = 8
    Top = 8
    Width = 72
    Height = 13
    Caption = #12496#12540#12398#20301#32622'(&P):'
    FocusControl = BarPosComboBox
  end
  object Bevel: TBevel
    Left = 0
    Top = 256
    Width = 281
    Height = 9
    Shape = bsTopLine
  end
  object BarPosComboBox: TComboBox
    Left = 8
    Top = 24
    Width = 129
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    Items.Strings = (
      #24038
      #19978
      #21491
      #19979)
  end
  object AutoDisplayCheckBox: TCheckBox
    Left = 8
    Top = 56
    Width = 265
    Height = 17
    Caption = #36984#25246#12373#12428#12383#32232#38598#12514#12540#12489#12391#33258#21205#30340#12395#34920#31034'(&A)'
    TabOrder = 1
    OnClick = AutoDisplayCheckBoxClick
  end
  object ModeListView: TListView
    Left = 8
    Top = 80
    Width = 265
    Height = 161
    Checkboxes = True
    Columns = <
      item
        AutoSize = True
        Caption = #32232#38598#12514#12540#12489
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    TabOrder = 2
    ViewStyle = vsReport
  end
  object OKButton: TButton
    Left = 104
    Top = 264
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CancelButton: TButton
    Left = 192
    Top = 264
    Width = 81
    Height = 25
    Cancel = True
    Caption = #12461#12515#12531#12475#12523
    ModalResult = 2
    TabOrder = 4
  end
end
