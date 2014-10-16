object Form1: TForm1
  Left = 260
  Top = 99
  Width = 719
  Height = 570
  Caption = 'Tabel Budget'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    711
    536)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 678
    Height = 504
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 329
      Top = 0
      Height = 504
    end
    object NiceGrid1: TNiceGrid
      Left = 332
      Top = 0
      Width = 346
      Height = 504
      Cursor = 101
      ColCount = 12
      RowCount = 20
      GridColor = clSilver
      HeaderLine = 2
      HeaderFont.Charset = DEFAULT_CHARSET
      HeaderFont.Color = clWindowText
      HeaderFont.Height = -11
      HeaderFont.Name = 'MS Sans Serif'
      HeaderFont.Style = []
      FooterFont.Charset = DEFAULT_CHARSET
      FooterFont.Color = clWindowText
      FooterFont.Height = -11
      FooterFont.Name = 'MS Sans Serif'
      FooterFont.Style = []
      Columns = <
        item
          Title = '0|Capital'
          Width = 80
          Color = 16775924
        end
        item
          Title = '0|Non Capital'
          Width = 80
          Color = 16773601
        end
        item
          Title = '2000|Capital'
          Width = 80
          Color = 16775924
        end
        item
          Title = '2000|Non Capital'
          Width = 80
          Color = 16773601
        end
        item
          Title = '2001|Capital'
          Width = 80
          Color = 16775924
        end
        item
          Title = '2001|Non Capital'
          Width = 80
          Color = 16773601
        end
        item
          Title = '2002|Capital'
          Width = 80
          Color = 16775924
        end
        item
          Title = '2002|Non Capital'
          Width = 80
          Color = 16773601
        end
        item
          Title = '2003|Capital'
          Width = 80
          Color = 16775924
        end
        item
          Title = '2003|Non Capital'
          Width = 80
          Color = 16773601
        end
        item
          Title = '2004|Capital'
          Width = 80
          Color = 16775924
        end
        item
          Title = '2004|Non Capital'
          Width = 80
          Color = 16773601
        end>
      GutterKind = gkNone
      GutterWidth = 40
      GutterFont.Charset = DEFAULT_CHARSET
      GutterFont.Color = clWindowText
      GutterFont.Height = -11
      GutterFont.Name = 'MS Sans Serif'
      GutterFont.Style = []
      ShowFooter = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Align = alClient
      TabOrder = 0
    end
    object NiceGridSync1: TNiceGridSync
      Left = 0
      Top = 0
      Width = 329
      Height = 504
      Cursor = 101
      ColCount = 3
      RowCount = 20
      AutoAddRow = True
      GridColor = clSilver
      HeaderLine = 2
      HeaderFont.Charset = DEFAULT_CHARSET
      HeaderFont.Color = clWindowText
      HeaderFont.Height = -11
      HeaderFont.Name = 'MS Sans Serif'
      HeaderFont.Style = []
      FooterFont.Charset = DEFAULT_CHARSET
      FooterFont.Color = clWindowText
      FooterFont.Height = -11
      FooterFont.Name = 'MS Sans Serif'
      FooterFont.Style = []
      FitToWidth = True
      Columns = <
        item
          Title = 'Unit Name'
          Width = 135
        end
        item
          Title = 'Unit Cost|Capital'
          Width = 80
          Color = 15790335
          CanResize = False
        end
        item
          Title = 'Unit Cost|Non Capital'
          Width = 80
          Color = 14671871
          CanResize = False
        end>
      GutterKind = gkNumber
      GutterWidth = 30
      GutterFont.Charset = DEFAULT_CHARSET
      GutterFont.Color = clWindowText
      GutterFont.Height = -11
      GutterFont.Name = 'MS Sans Serif'
      GutterFont.Style = []
      ShowFooter = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Align = alLeft
      TabOrder = 1
      Grid = NiceGrid1
    end
  end
end
