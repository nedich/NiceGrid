object Form1: TForm1
  Left = 252
  Top = 121
  Caption = 'NiceGrid Demo - priyatna.org'
  ClientHeight = 515
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  DesignSize = (
    622
    515)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 199
    Height = 13
    Caption = '- Try to copy paste a block between Excel'
  end
  object Label2: TLabel
    Left = 16
    Top = 32
    Width = 307
    Height = 13
    Caption = 
      '- Try type something then press * and Enter. Try also > and Ente' +
      'r.'
  end
  object NiceGrid1: TNiceGrid
    Left = 17
    Top = 62
    Width = 597
    Height = 371
    Cursor = 1
    ColCount = 5
    RowCount = 20
    AutoAddRow = True
    DefColWidth = 100
    GridColor = clSilver
    ShowGrid = False
    HeaderLine = 2
    HeaderColor = 14614528
    HeaderLightColor = 16744448
    HeaderDarkColor = clBlack
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWhite
    HeaderFont.Height = -11
    HeaderFont.Name = 'MS Sans Serif'
    HeaderFont.Style = []
    FooterFont.Charset = DEFAULT_CHARSET
    FooterFont.Color = clRed
    FooterFont.Height = -11
    FooterFont.Name = 'MS Sans Serif'
    FooterFont.Style = []
    Columns = <
      item
        Title = 'Merged;Multilined|Merged;Multilined'
        Footer = 'Footer 0'
        Width = 100
        CanResize = False
        ReadOnly = True
        EditorType = nietEdit
      end
      item
        Title = 'First Group|One'
        Footer = 'Footer 1'
        Width = 100
        Color = 14024703
        HorzAlign = haCenter
        Strings.Strings = (
          'Satu'
          'Dua'
          'Tiga')
        EditorType = nietCombo
      end
      item
        Title = 'First Group|Two'
        Footer = 'Footer 2'
        Width = 100
        EditorType = nietEdit
      end
      item
        Title = 'Second Group|One'
        Footer = 'Footer 3'
        Width = 100
        Color = clWhite
        HorzAlign = haRight
        EditorType = nietEdit
      end
      item
        Title = 'Second Group|Two'
        Footer = 'Footer 4'
        Width = 100
        HorzAlign = haCenter
        EditorType = nietEdit
      end>
    GutterKind = gkNumber
    GutterWidth = 40
    GutterFont.Charset = DEFAULT_CHARSET
    GutterFont.Color = clWhite
    GutterFont.Height = -11
    GutterFont.Name = 'MS Sans Serif'
    GutterFont.Style = []
    ShowFooter = True
    OnDrawHeader = NiceGrid1DrawHeader
    OnInsertRow = NiceGrid1InsertRow
    OnEditorCreated = NiceGrid1EditorCreated
    Options = [ngoThemed]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 15
    Top = 440
    Width = 49
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Flat'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 88
    Top = 440
    Width = 89
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'System Colors'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 192
    Top = 440
    Width = 81
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Fit to Width'
    TabOrder = 3
    OnClick = CheckBox3Click
  end
  object CheckBox4: TCheckBox
    Left = 288
    Top = 440
    Width = 121
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Auto Column Width'
    TabOrder = 4
    OnClick = CheckBox4Click
  end
  object CheckBox5: TCheckBox
    Left = 424
    Top = 440
    Width = 81
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Show Grids'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBox5Click
  end
  object Button1: TButton
    Left = 272
    Top = 483
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Hide 3rd Column'
    TabOrder = 8
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 483
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Insert New Row'
    TabOrder = 6
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 144
    Top = 483
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Delete Current Row'
    TabOrder = 7
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 416
    Top = 483
    Width = 161
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Toggle ReadOnly 3rd Column'
    TabOrder = 9
    OnClick = Button4Click
  end
  object CheckBox6: TCheckBox
    Left = 528
    Top = 440
    Width = 89
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Show Footer'
    TabOrder = 10
    OnClick = CheckBox6Click
  end
  object xbThemed: TCheckBox
    Left = 16
    Top = 460
    Width = 89
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Themed'
    Checked = True
    State = cbChecked
    TabOrder = 11
    OnClick = xbThemedClick
  end
  object xbExcel: TCheckBox
    Left = 88
    Top = 460
    Width = 89
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Excel'
    TabOrder = 12
    OnClick = xbExcelClick
  end
  object xbMultiselect: TCheckBox
    Left = 192
    Top = 460
    Width = 89
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Multiselect'
    TabOrder = 13
    OnClick = xbMultiselectClick
  end
  object xbEnabled: TCheckBox
    Left = 288
    Top = 460
    Width = 89
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Enabled'
    Checked = True
    State = cbChecked
    TabOrder = 14
    OnClick = xbEnabledClick
  end
end
