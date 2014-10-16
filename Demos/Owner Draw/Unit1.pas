unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, NiceGrid;

type
  TForm1 = class(TForm)
    NiceGrid1: TNiceGrid;
    procedure FormCreate(Sender: TObject);
    procedure NiceGrid1DrawCell(Sender: TObject; ACanvas: TCanvas; X,
      Y: Integer; Rc: TRect; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  x: Integer;
begin
  for x := 0 to 19 do
  begin
    NiceGrid1.Cells[0, x] := IntToStr(Random(100));
    NiceGrid1.Cells[1, x] := IntToStr(Random(100));
    NiceGrid1.Cells[2, x] := IntToStr(Random(100));
    NiceGrid1.Cells[3, x] := IntToStr(Random(100));
    NiceGrid1.Cells[4, x] := IntToStr(Random(100));
  end;
end;

procedure TForm1.NiceGrid1DrawCell(Sender: TObject; ACanvas: TCanvas; X,
  Y: Integer; Rc: TRect; var Handled: Boolean);
var
  i: Integer;
begin
  i := StrToIntDef(NiceGrid1.Cells[X, Y], 0);
  if Odd(i)
    then ACanvas.Font.Color := clRed;
  if ((i mod 10) = 0)
    then ACanvas.Brush.Color := clYellow;
end;

end.
