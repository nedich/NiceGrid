object Form1: TForm1
  Left = 308
  Top = 139
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Customizing Cells Based on Conditions'
  ClientHeight = 419
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object NiceGrid1: TNiceGrid
    Left = 16
    Top = 16
    Width = 441
    Height = 385
    Cursor = 101
    ColCount = 5
    RowCount = 20
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
        Title = 'Column 1'
        Width = 84
      end
      item
        Title = 'Column 2'
        Width = 84
      end
      item
        Title = 'Column 3'
        Width = 83
      end
      item
        Title = 'Column 4'
        Width = 83
      end
      item
        Title = 'Column 5'
        Width = 83
      end>
    GutterFont.Charset = DEFAULT_CHARSET
    GutterFont.Color = clWindowText
    GutterFont.Height = -11
    GutterFont.Name = 'MS Sans Serif'
    GutterFont.Style = []
    ShowFooter = False
    OnDrawCell = NiceGrid1DrawCell
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TabOrder = 0
  end
end
