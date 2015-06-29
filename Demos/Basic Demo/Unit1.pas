unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, NiceGrid;

type
  TForm1 = class(TForm)
    NiceGrid1: TNiceGrid;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox6: TCheckBox;
    xbThemed: TCheckBox;
    xbExcel: TCheckBox;
    xbMultiselect: TCheckBox;
    xbEnabled: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NiceGrid1DrawHeader(Sender: TObject; ACanvas: TCanvas;
      Rc: TRect; Str: string; var Handled: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure NiceGrid1InsertRow(Sender: TObject; ARow: Integer);
    procedure Button4Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure NiceGrid1EditorCreated(AGrid: TNiceGrid; EditorControl: TWinControl);
    procedure xbThemedClick(Sender: TObject);
    procedure xbExcelClick(Sender: TObject);
    procedure xbMultiselectClick(Sender: TObject);
    procedure xbEnabledClick(Sender: TObject);
    procedure NiceGrid1CellChanging(Sender: TObject; Col, Row: Integer; var CanChange: Boolean);
    procedure NiceGrid1FormatText(Grid: TNiceGrid; X,Y: Integer; var CellText: string);
    procedure NiceGrid1DrawBackground(Sender: TObject; ACanvas: TCanvas; X, Y: Integer; Rc: TRect;
      var Handled: Boolean);
    procedure NiceGrid1EditorCreating(Sender: TObject);
    procedure NiceGrid1CellAssignment(Sender: TObject; Col, Row: Integer; var Str: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  u_utlClassHelpers;

{$R *.dfm}
{$R XPTheme.res}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  NiceGrid1.Flat := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
  begin
    with NiceGrid1 do
    begin
      //GridColor := clBtnShadow;
      HeaderColor := clBtnFace;
      HeaderDarkColor := clBtnShadow;
      HeaderLightColor := clBtnHighlight;
      HeaderFont.Color := clBlack;
      GutterFont.Color := clBlack;
    end;
  end else
  begin
    with NiceGrid1 do
    begin
      //GridColor := clGray;
      HeaderColor := $00DF0000;
      HeaderDarkColor := clBlack;
      HeaderLightColor := $00FF8000;
      HeaderFont.Color := clWhite;
      GutterFont.Color := clWhite;
    end;
  end;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  NiceGrid1.FitToWidth := CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  NiceGrid1.AutoColWidth := CheckBox4.Checked;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  NiceGrid1.ShowGrid := CheckBox5.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  x: Integer;
begin
  NiceGrid1.BeginUpdate;
  for x := 0 to 9 do
  begin
    NiceGrid1[0, x] := 'Sample Text';
    NiceGrid1[1, x] := 'Centered Text';
    NiceGrid1[2, x] := 'Left Alignment';
    NiceGrid1[3, x] := FormatFloat('###,###,##0.##', Random(20000000));
    NiceGrid1[4, x] := IntToStr(Random(2000));
  end;
  NiceGrid1.cells[3,6] := 'ReadOnly';
  NiceGrid1.EndUpdate;
  CheckBox2Click(nil);
end;

procedure TForm1.NiceGrid1CellAssignment(Sender: TObject; Col, Row: Integer; var Str: string);
begin
  str := 'edited';
end;

procedure TForm1.NiceGrid1CellChanging(Sender: TObject; Col, Row: Integer; var CanChange: Boolean);
begin
  //
end;

procedure TForm1.NiceGrid1DrawBackground(Sender: TObject; ACanvas: TCanvas; X, Y: Integer;
  Rc: TRect; var Handled: Boolean);
begin
  if(x=3) and (y=6) then
    ACanvas.Brush.color := clSilver; //colorize as readonly
end;

procedure TForm1.NiceGrid1DrawHeader(Sender: TObject; ACanvas: TCanvas;
  Rc: TRect; Str: String; var Handled: Boolean);
begin
  if (Str = 'One')
    then ACanvas.Font.Color := clRed;
end;




procedure TForm1.NiceGrid1EditorCreated(AGrid: TNiceGrid; EditorControl: TWinControl);
{-----------------------------------------------------------------------------
  Procedure: NiceGrid1EditorCreated
  Author:    nbi
  Date:      16-Oct-2014
  Arguments: AGrid: TNiceGrid; EditorControl: TWinControl
  Result:    None
-----------------------------------------------------------------------------}
begin
  if(EditorControl is TComboBox) then
    with TComboBox(EditorControl), Items do begin
      Add('test 1');
      Add('test 2');      
      Add('test 3');
      SelectItem(AGrid.cells[AGrid.Col, AGrid.Row]);
    end;
end;




procedure TForm1.NiceGrid1EditorCreating(Sender: TObject);
begin
  if(NiceGrid1.Col=3) and (NiceGrid1.Row=6) then
    ABORT;
end;

procedure TForm1.NiceGrid1FormatText(Grid: TNiceGrid; X,Y: Integer; var CellText: string);
{-----------------------------------------------------------------------------
  Procedure: NiceGrid1FormatText
  Author:    nbi
  Date:      16-Oct-2014
  Arguments: Grid: TNiceGrid; var CellText: string
  Result:    None
-----------------------------------------------------------------------------}
var
  m: extended;
begin
  if(x=4) then try
    if(trystrtofloat(celltext, m)) then
      celltext := format('%m',[m]);
  except
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  NiceGrid1.Columns[2].Visible := not NiceGrid1.Columns[2].Visible;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  NiceGrid1.InsertRow(NiceGrid1.Row);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  NiceGrid1.DeleteRow(NiceGrid1.Row);
end;

procedure TForm1.NiceGrid1InsertRow(Sender: TObject; ARow: Integer);
begin
  NiceGrid1.Cells[0, ARow] := 'New Row';
end;



procedure TForm1.xbEnabledClick(Sender: TObject);
{-----------------------------------------------------------------------------
  Procedure: xbEnabledClick
  Author:    nbi
  Date:      16-Oct-2014
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}
begin
  NiceGrid1.Enabled := xbEnabled.Checked;
end;




procedure TForm1.xbExcelClick(Sender: TObject);
{-----------------------------------------------------------------------------
  Procedure: xbExcelClick
  Author:    nbi
  Date:      16-Oct-2014
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}
begin
  if(xbExcel.Checked) then 
    NiceGrid1.Options := NiceGrid1.Options + [ngoExcel]
  else
    NiceGrid1.Options := NiceGrid1.Options - [ngoExcel]
end;




procedure TForm1.xbMultiselectClick(Sender: TObject);
{-----------------------------------------------------------------------------
  Procedure: xbMultiselectClick
  Author:    nbi
  Date:      16-Oct-2014
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}
begin
  if(xbMultiselect.Checked) then 
    NiceGrid1.Options := NiceGrid1.Options + [ngoMultiCellSelect]
  else
    NiceGrid1.Options := NiceGrid1.Options - [ngoMultiCellSelect]
end;




procedure TForm1.xbThemedClick(Sender: TObject);
{-----------------------------------------------------------------------------
  Procedure: xbThemedClick
  Author:    nbi
  Date:      16-Oct-2014
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}
begin
  if(xbThemed.Checked) then 
    NiceGrid1.Options := NiceGrid1.Options + [ngoThemed]
  else
    NiceGrid1.Options := NiceGrid1.Options - [ngoThemed];
end;



procedure TForm1.Button4Click(Sender: TObject);
begin
  NiceGrid1.Columns[2].ReadOnly := not NiceGrid1.Columns[2].ReadOnly;
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
  NiceGrid1.ShowFooter := not NiceGrid1.ShowFooter;
end;

end.
