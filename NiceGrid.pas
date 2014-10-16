
{-------------------------------------------------------------------------------

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.


     The Original Code is NiceGrid.pas released at April 11st, 2003.
     The Original Code is a part of NiceGrid component.
     The Initial Developer of the Original Code is Priyatna.
     (Website: http://www.priyatna.org/ Email: me@priyatna.org)
     All Rights Reserved.

     Contributors:
       - C. S. Phua <csphua@teledynamics.com.my>


Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}


unit NiceGrid;

interface

uses
  Windows, Forms, Controls, Messages, SysUtils, Classes, Graphics, Contnrs,
  StdCtrls, ExtCtrls, Clipbrd;

type
  PHeaderInfo = ^THeaderInfo;
  THeaderInfo = record
    Str: string;
    Rc: TRect;
  end;

  THorzAlign = (haLeft, haCenter, haRight);
  TVertAlign = (vaTop, vaCenter, vaBottom);
  TGutterKind = (gkNone, gkBlank, gkPointer, gkNumber, gkString);
  TGridHittest = (gtNone, gtLeftTop, gtLeft, gtTop, gtCell, gtColSizing, gtSmallBox);

  TNiceGrid = class;

  TNiceColumn = class(TCollectionItem)
  private
    FTitle: string;
    FFooter: string;
    FWidth: Integer;
    FFont: TFont;
    FColor: TColor;
    FHorzAlign: THorzAlign;
    FVertAlign: TVertAlign;
    FVisible: Boolean;
    FStrings: TStrings;
    FTag: Integer;
    FTag2: Integer;
    FCanResize: Boolean;
    FHint: string;
    FReadOnly: Boolean;
    function GetGrid: TNiceGrid;
    function IsFontStored: Boolean;
    procedure FontChange(Sender: TObject);
    procedure SetTitle(Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetFont(Value: TFont);
    procedure SetColor(Value: TColor);
    procedure SetHorzAlign(Value: THorzAlign);
    procedure SetVertAlign(Value: TVertAlign);
    procedure SetVisible(Value: Boolean);
    procedure SetStrings(Value: TStrings);
    procedure SetFooter(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Grid: TNiceGrid read GetGrid;
    property Title: string read FTitle write SetTitle;
    property Footer: string read FFooter write SetFooter;
    property Width: Integer read FWidth write SetWidth;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Color: TColor read FColor write SetColor default clWindow;
    property HorzAlign: THorzAlign read FHorzAlign write SetHorzAlign default haLeft;
    property VertAlign: TVertAlign read FVertAlign write SetVertAlign default vaCenter;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Tag: Integer read FTag write FTag default 0;
    property Tag2: Integer read FTag2 write FTag2 default 0;
    property Hint: string read FHint write FHint;
    property Strings: TStrings read FStrings write SetStrings;
    property CanResize: Boolean read FCanResize write FCanResize default True;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
  end;


  TNiceColumns = class(TCollection)
  private
    FGrid: TNiceGrid;
    function GetItem(Index: Integer): TNiceColumn;
    procedure SetItem(Index: Integer; Value: TNiceColumn);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AGrid: TNiceGrid);
    property Grid: TNiceGrid read FGrid;
    property Items[Index: Integer]: TNiceColumn read GetItem write SetItem; default;
    function Add: TNiceColumn;
    function AddItem(Item: TNiceColumn; Index: Integer): TNiceColumn;
    function Insert(Index: Integer): TNiceColumn;
  end;


  TNiceInplace = class(TEdit)
  private
    FGrid: TNiceGrid;
    FAlignment: THorzAlign;
    CellX, CellY: Integer;
    procedure SetAlignment(Value: THorzAlign);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(Grid: TNiceGrid); reintroduce;
    procedure ShowEdit(X, Y: Integer);
    procedure HideEdit;
  end;

  TMergeCell = class(TObject)
  public
    Caption: string;
    Rc: TRect;
    Color: TColor;
    Font: TFont;
    HorzAlign: THorzAlign;
    VertAlign: TVertAlign;
    constructor Create;
    destructor Destroy; override;
  end;

  TOnDrawCellEvent = procedure (Sender: TObject; ACanvas: TCanvas; X, Y: Integer;
    Rc: TRect; var Handled: Boolean) of object;

  TOnDrawHeaderEvent = procedure (Sender: TObject; ACanvas: TCanvas; Rc: TRect;
    Str: string; var Handled: Boolean) of object;

  TOnFilterChar = procedure (Sender: TObject; Col: Integer; Row: Integer;
    Chr: Char; var Allowed: Boolean) of object;

  TOnHeaderClick = procedure (Sender: TObject; Col: Integer;
    Button: TMouseButton; Shift: TShiftState) of object;

  TOnGutterClick = procedure (Sender: TObject; Row: Integer;
    Button: TMouseButton; Shift: TShiftState) of object;

  TOnCellAssignment = procedure (Sender: TObject; Col, Row: Integer;
    var Str: string) of object;

  TOnCellChange = procedure (Sender: TObject; Col, Row: Integer; var Str: string)
    of object;

  TOnCellChanging = procedure (Sender: TObject; Col, Row: Integer;
    var CanChange: Boolean) of object;

  TOnRowEvent = procedure (Sender: TObject; ARow: Integer) of object;

  TOnColRowChanged = procedure (Sender: TObject; Col, Row: Integer) of object;

  TNiceGridSync = class;

  TNiceGrid = class(TCustomPanel)
  private
    ForcedColumn: Integer;
    FixedWidth, FixedHeight: Integer;
    BodyWidth, BodyHeight: Integer;
    AllWidth, AllHeight: Integer;
    FooterTop: Integer;
    CellBox: TRect;

    FHorzOffset: Integer;
    FVertOffset: Integer;
    FMaxHScroll: Integer;
    FMaxVScroll: Integer;
    FSmallChange: Integer;
    FLargeChange: Integer;

    FAutoAddRow: Boolean;
    FRowCount: Integer;
    FDefRowHeight: Integer;
    FDefColWidth: Integer;
    FFlat: Boolean;

    FHeaderLine: Integer;
    FHeaderInfos: TList;
    FUpdating: Boolean;
    FColor: TColor;
    FAlternateColor: TColor;
    FGridColor: TColor;
    FShowGrid: Boolean;
    FHeaderColor: TColor;
    FHeaderLightColor: TColor;
    FHeaderDarkColor: TColor;
    FSelectionColor: TColor;
    FHeaderFont: TFont;
    FGutterFont: TFont;

    FGutterKind: TGutterKind;
    FGutterWidth: Integer;

    FFitToWidth: Boolean;
    FAutoColWidth: Boolean;
    FReadOnly: Boolean;
    FColumns: TNiceColumns;

    ValidationEnabled: Boolean;
    FEdit: TNiceInplace;
    FCol: Integer;
    FRow: Integer;
    FCol2, FRow2: Integer; // Selection
    FSelectArea: TRect;

    SmallBox: TRect;
    SmallBoxArea: TRect;
    SmallBoxPos: Byte;

    BuffString: string;
    IsEditing: Boolean;
    SizingCol: Integer;
    SizingColX: Integer;
    LastHover: Integer;
    Sync: TNiceGridSync;
    Mergeds: TList;

    FOnDrawCell: TOnDrawCellEvent;
    FOnDrawHeader: TOnDrawHeaderEvent;
    FOnDrawGutter: TOnDrawHeaderEvent;
    FOnDrawFooter: TOnDrawHeaderEvent;
    FOnFilterChar: TOnFilterChar;
    FOnHeaderClick: TOnHeaderClick;
    FOnGutterClick: TOnGutterClick;
    FOnCellChange: TOnCellChange;
    FOnCellChanging: TOnCellChanging;
    FOnColRowChanged: TOnColRowChanged;
    FOnInsertRow: TOnRowEvent;
    FOnDeleteRow: TOnRowEvent;
    FOnCellAssignment: TOnCellAssignment;
    FGutterStrings: TStrings;
    FShowFooter: Boolean;
    FFooterFont: TFont;
    FEnabled: Boolean;
    FAutoFillRight: Boolean;
    FAutoFillDown: Boolean;

    procedure WMUnknown(var Msg: TMessage); message WM_USER + $B902;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;

    procedure CMWantSpecialKey(var Message: TWMKey); message CM_WANTSPECIALKEY;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;

    function TotalWidth: Integer;
    procedure ClearHeaderInfos;

    procedure ClearUnused;
    procedure RenderGutter;
    procedure RenderHeader;
    procedure DrawSelection;

    procedure SetHorzOffset(Value: Integer);
    procedure SetVertOffset(Value: Integer);
    function GetColCount: Integer;
    procedure SetColCount(Value: Integer);
    procedure SetRowCount(Value: Integer);
    procedure SetDefColWidth(Value: Integer);
    procedure SetDefRowHeight(Value: Integer);
    procedure SetFlat(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetAlternateColor(Value: TColor);
    procedure SetGridColor(Value: TColor);
    procedure SetShowGrid(Value: Boolean);
    procedure SetHeaderLine(Value: Integer);
    procedure SetHeaderColor(Value: TColor);
    procedure SetHeaderLightColor(Value: TColor);
    procedure SetHeaderDarkColor(Value: TColor);
    procedure SetHeaderFont(Value: TFont);
    procedure SetSelectionColor(Value: TColor);
    procedure SetFitToWidth(Value: Boolean);
    procedure SetAutoColWidth(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure InternalSetCell(X, Y: Integer; Value: string; FireOnChange: Boolean);
    procedure SetCell(X, Y: Integer; Value: string);
    function GetColWidths(Index: Integer): Integer;
    procedure SetColWidths(Index: Integer; Value: Integer);
    procedure SetColumns(Value: TNiceColumns);
    procedure SetCol(Value: Integer);
    procedure SetRow(Value: Integer);
    procedure AdjustSelection(Value: TRect; Force: Boolean);
    procedure SetSelectArea(Value: TRect);
    procedure SetGutterKind(Value: TGutterKind);
    procedure SetGutterWidth(Value: Integer);
    procedure SetGutterFont(Value: TFont);
    procedure HeaderFontChange(Sender: TObject);
    procedure GutterFontChange(Sender: TObject);

    function CreateColumn: TNiceColumn;
    procedure UpdateColumn(Index: Integer);
    procedure UpdateColumns;
    procedure UpdateHeader;

    function GetCellRect(x, y: Integer): TRect;
    function CellRectToClient(R: TRect): TRect;
    function GetCellAtPos(X, Y: Integer): TPoint;
    function GetColFromX(X: Integer): Integer;
    function GetRowFromY(Y: Integer): Integer;
    function GetColCoord(I: Integer): Integer;
    function GetCell(X, Y: Integer): string;
    function SafeGetCell(X, Y: Integer): string;
    function GetCellColor(X, Y: Integer): TColor;
    procedure DrawCell(X, Y: Integer);
    function FastDrawCell(X, Y: Integer; IsEditing: Boolean): TPoint;
    procedure ForceHideCaret;
    procedure ForceShowCaret;
    procedure NormalizeVertOffset;
    procedure InvalidateCells;
    procedure InvalidateRightWard(Left: Integer);
    procedure InvalidateDownWard(Top: Integer);
    procedure InvalidateHeader;
    procedure InvalidateGutter;
    function GetFirstVisible: Integer;
    function GetLastVisible: Integer;
    function GetNextVisible(Index: Integer): Integer;
    function GetPrevVisible(Index: Integer): Integer;
    procedure ColRowChanged;
    procedure SetGutterStrings(const Value: TStrings);
    function GetObject(X, Y: Integer): TObject;
    procedure SetObject(X, Y: Integer; const Value: TObject);
    procedure BuildMergeData;
    procedure DrawMergedCell(Index: Integer);
    procedure SetShowFooter(const Value: Boolean);
    procedure RenderFooter;
    procedure SetFooterFont(const Value: TFont);
    procedure FooterFontChange(Sender: TObject);
    procedure DrawFixCell(Rc: TRect; Str: string; AFont: TFont; AEvent: TOnDrawHeaderEvent);
    procedure SetEnabled(const Value: Boolean); reintroduce;

  protected
    function GetMergedCellsData: TList;
    function GetHeaderInfo: TList;
    procedure SetScrollBar(AKind, AMax, APos, AMask: Integer); virtual;
    procedure ShowHideScrollBar(HorzVisible, VertVisible: Boolean); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Recalculate; virtual;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    property Cells[X, Y: Integer]: string read GetCell write SetCell; default;
    property Objects[X, Y: Integer]: TObject read GetObject write SetObject;
    property ColWidths[Index: Integer]: Integer read GetColWidths write SetColWidths;
    procedure EnsureVisible(X, Y: Integer); overload;
    procedure CutToClipboard;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    function GetHitTestInfo(X, Y: Integer): TGridHitTest;
    function HeaderCellsCount: Integer;
    function HeaderCells(I: Integer): THeaderInfo;
    property Col: Integer read FCol write SetCol;
    property Row: Integer read FRow write SetRow;
    property SelectArea: TRect read FSelectArea write SetSelectArea;
    procedure DeleteRow(ARow: Integer);
    procedure InsertRow(ARow: Integer);
    function AddRow: Integer;
    property HorzOffset: Integer read FHorzOffset write SetHorzOffset;
    property VertOffset: Integer read FVertOffset write SetVertOffset;
    function MergeCells(const X1, Y1, X2, Y2: Integer; ACaption: string): TMergeCell;
    procedure ClearMergeCells;

  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property ColCount: Integer read GetColCount write SetColCount;
    property RowCount: Integer read FRowCount write SetRowCount default 5;
    property AutoAddRow: Boolean read FAutoAddRow write FAutoAddRow default False;
    property AutoFillDown: Boolean read FAutoFillDown write FAutoFillDown default False;
    property AutoFillRight: Boolean read FAutoFillRight write FAutoFillRight default False;
    property DefRowHeight: Integer read FDefRowHeight write SetDefRowHeight default 18;
    property DefColWidth: Integer read FDefColWidth write SetDefColWidth default 80;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Color: TColor read FColor write SetColor default clWindow;
    property AlternateColor: TColor read FAlternateColor write SetAlternateColor default clWindow;
    property GridColor: TColor read FGridColor write SetGridColor default clBtnFace;
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default True;
    property HeaderLine: Integer read FHeaderLine write SetHeaderLine default 1;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor default clBtnFace;
    property HeaderLightColor: TColor read FHeaderLightColor write SetHeaderLightColor default clBtnHighlight;
    property HeaderDarkColor: TColor read FHeaderDarkColor write SetHeaderDarkColor default clBtnShadow;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property FooterFont: TFont read FFooterFont write SetFooterFont;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default $00CAFFFF;
    property FitToWidth: Boolean read FFitToWidth write SetFitToWidth default False;
    property AutoColWidth: Boolean read FAutoColWidth write SetAutoColWidth default False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Columns: TNiceColumns read FColumns write SetColumns;
    property GutterKind: TGutterKind read FGutterKind write SetGutterKind default gkBlank;
    property GutterWidth: Integer read FGutterWidth write SetGutterWidth default 20;
    property GutterFont: TFont read FGutterFont write SetGutterFont;
    property GutterStrings: TStrings read FGutterStrings write SetGutterStrings;
    property ShowFooter: Boolean read FShowFooter write SetShowFooter;
    property OnDrawCell: TOnDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnDrawHeader: TOnDrawHeaderEvent read FOnDrawHeader write FOnDrawHeader;
    property OnDrawGutter: TOnDrawHeaderEvent read FOnDrawGutter write FOnDrawGutter;
    property OnDrawFooter: TOnDrawHeaderEvent read FOnDrawFooter write FOnDrawFooter;
    property OnFilterChar: TOnFilterChar read FOnFilterChar write FOnFilterChar;
    property OnHeaderClick: TOnHeaderClick read FOnHeaderClick write FOnHeaderClick;
    property OnGutterClick: TOnGutterClick read FOnGutterClick write FOnGutterClick;
    property OnCellChange: TOnCellChange read FOnCellChange write FOnCellChange;
    property OnCellChanging: TOnCellChanging read FOnCellChanging write FOnCellChanging;
    property OnColRowChanged: TOnColRowChanged read FOnColRowChanged write FOnColRowChanged;
    property OnInsertRow: TOnRowEvent read FOnInsertRow write FOnInsertRow;
    property OnDeleteRow: TOnRowEvent read FOnDeleteRow write FOnDeleteRow;
    property OnCellAssignment: TOnCellAssignment read FOnCellAssignment write FOnCellAssignment;
    property Font;
    property Anchors;
    property Align;
    property BevelKind;
    property BorderStyle default bsSingle;
    property BevelOuter default bvNone;
    property BevelInner;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property PopupMenu;
  end;

  TNiceGridSync = class(TNiceGrid)
  private
    FGrid: TNiceGrid;
    procedure SetGrid(const Value: TNiceGrid);
    procedure SyncDeleteRow(Sender: TObject; ARow: Integer);
    procedure SyncInsertRow(Sender: TObject; ARow: Integer);
    procedure SyncColRow(Sender: TObject; ACol, ARow: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetScrollBar(AKind, AMax, APos, AMask: Integer); override;
    procedure ShowHideScrollBar(HorzVisible, VertVisible: Boolean); override;
    property OnDeleteRow;
    property OnInsertRow;
    property OnColRowChanged;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Grid: TNiceGrid read FGrid write SetGrid;
  end;


  function DrawString(Canvas: TCanvas; Str: string; Rc: TRect;
    HorzAlign: THorzAlign; VertAlign: TVertAlign; IsEditing: Boolean): TPoint;

  procedure DrawStringMulti(Canvas: TCanvas; Str: string; Rc: TRect;
    HorzAlign: THorzAlign; VertAlign: TVertAlign);


implementation


{$R NiceCursors.res}

uses
  Math;

const
  crPlus = 101;
  crSmallCross = 102;
  crRight = 103;
  crDown = 104;
  crLeftTop = 105;

  CursorArray: array [TGridHitTest] of TCursor =
  //(gtNone, gtLeftTop, gtLeft, gtTop, gtCell, gtColSizing, gtSmallBox);
    (crDefault, crLeftTop, crRight, crDown, crPlus, crHSplit, crSmallCross);

  MergeID = -2;
    
    
{ TNiceGrid }

constructor TNiceGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 200;
  inherited Color := clWindow;
  BevelOuter := bvNone;
  BorderStyle := bsSingle;
  TabStop := True;
  TabOrder := 0;
  ParentColor := False;
  ParentBackground := False;
  ParentFont := False;

  {$IFDEF VER150}
  ControlStyle := ControlStyle + [csNeedsBorderPaint];
  {$ENDIF}

  FFlat := True;
  FEnabled := True;
  FColor := clWindow;
  FAlternateColor := clWindow;
  FGridColor := clBtnFace;
  FShowGrid := True;
  FHeaderColor := clBtnface;
  FHeaderLightColor := clBtnHighlight;
  FHeaderDarkColor := clBtnShadow;
  FHeaderFont := TFont.Create;
  FHeaderFont.OnChange := HeaderFontChange;
  FSelectionColor := $00CAFFFF;

  FFooterFont := TFont.Create;
  FFooterFont.OnChange := FooterFontChange;

  FDefRowHeight := 18;
  FDefColWidth := 80;
  FRowCount := 5;
  FAutoAddRow := False;
  FGutterKind := gkBlank;
  FGutterWidth := 20;
  FGutterFont := TFont.Create;
  FGutterFont.OnChange := GutterFontChange;

  FHorzOffset  := 0;
  FVertOffset  := 0;
  FMaxHScroll  := 0;
  FMaxVScroll  := 0;
  FSmallChange := FDefRowHeight;
  FLargeChange := FDefRowHeight * 5;
  ForcedColumn := -1;
  AllWidth := 200;
  AllHeight := 200;

  FHeaderLine := 1;
  FHeaderInfos := TList.Create;

  ValidationEnabled := True;
  CellBox := Rect(0, 0, 0, 0);
  FCol := 0;
  FRow := 0;
  FCol2 := 0;
  FRow2 := 0;
  FSelectArea := Rect(0, 0, 0, 0);
  IsEditing := False;
  BuffString := '';
  SmallBox := Rect(-1, -1, -1, -1);
  SmallBoxArea := Rect(-1, -1, -1, -1);
  SmallBoxPos := 0;
  SizingCol := -1;
  SizingColX := -1;

  Screen.Cursors[crPlus] := LoadCursor(hinstance, 'CR_PLUS');
  Screen.Cursors[crSmallCross] := LoadCursor(hInstance, 'CR_CROSS');
  Screen.Cursors[crRight] := LoadCursor(hinstance, 'CR_RIGHT');
  Screen.Cursors[crDown] := LoadCursor(hinstance, 'CR_DOWN');
  Screen.Cursors[crLeftTop] := LoadCursor(hinstance, 'CR_LEFTTOP');
  Cursor := crPlus;

  FColumns := TNiceColumns.Create(Self);
  FEdit := TNiceInplace.Create(Self);
  FGutterStrings := TStringList.Create;
  Mergeds := TList.Create;

end;

destructor TNiceGrid.Destroy;
begin
  ClearMergeCells;
  Mergeds.Free;
  FGutterStrings.Free;
  FEdit.Free;
  FColumns.Free;
  ClearHeaderInfos;
  FHeaderInfos.Free;
  FHeaderFont.Free;
  FFooterFont.Free;
  FGutterFont.Free;
  inherited Destroy;
end;

procedure TNiceGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_HSCROLL or WS_VSCROLL;
end;

procedure TNiceGrid.CreateWnd;
begin
  inherited CreateWnd;
  ShowHideScrollBar(False, False);
  Recalculate;
end;

procedure TNiceGrid.SetScrollBar(AKind, AMax, APos, AMask: Integer);
var Info: TScrollInfo;
begin
  FillChar(Info, SizeOf(TScrollInfo), 0);
  Info.cbSize := SizeOf(TScrollInfo);
  Info.nMin := 0;
  Info.nMax := AMax;
  Info.nPos := APos;
  Info.fMask := AMask;
  SetScrollInfo(Handle, AKind, Info, TRUE);
  if (AKind = SB_VERT) and Assigned(Sync) then
  begin
    if ((AMask and SIF_RANGE) <> 0)
      then Sync.FMaxVScroll := AMax;
    if ((AMask and SIF_POS) <> 0)
      then Sync.VertOffset := APos;
  end;
end;

procedure TNiceGrid.ShowHideScrollBar(HorzVisible, VertVisible: Boolean);
begin
  ShowScrollBar(Handle, SB_HORZ, HorzVisible);
  ShowScrollBar(Handle, SB_VERT, VertVisible);
end;

procedure TNiceGrid.WMHScroll(var Msg: TWMVScroll);
var
  Old: Integer;
begin
  ForceHideCaret;
  Old := FHorzOffset;
  case Msg.ScrollCode of
    SB_LINELEFT:
      FHorzOffset := FHorzOffset - FSmallChange;
    SB_LINERIGHT:
      FHorzOffset := FHorzOffset + FSmallChange;
    SB_PAGELEFT:
      FHorzOffset := FHorzOffset - FLargeChange;
    SB_PAGERIGHT:
      FHorzOffset := FHorzOffset + FLargeChange;
    SB_THUMBTRACK:
      FHorzOffset := Msg.Pos;
    SB_THUMBPOSITION:
      FHorzOffset := Msg.Pos;
  end;
  FHorzOffset := Max(0, Min(FMaxHScroll, FHorzOffset));
  if (FHorzOffset <> Old) then
  begin
    SetScrollBar(SB_HORZ, 0, FHorzOffset, SIF_POS);
    InvalidateRightWard(FixedWidth);
  end;
end;

procedure TNiceGrid.WMVScroll(var Msg: TWMHScroll);
var
  Old: Integer;
begin
  ForceHideCaret;
  Old := FVertOffset;
  case Msg.ScrollCode of
    SB_LINEUP:
      FVertOffset := FVertOffset - FSmallChange;
    SB_LINEDOWN:
      FVertOffset := FVertOffset + FSmallChange;
    SB_PAGEUP:
      FVertOffset := FVertOffset - FLargeChange;
    SB_PAGEDOWN:
      FVertOffset := FVertOffset + FLargeChange;
    SB_THUMBTRACK:
      FVertOffset := Msg.Pos;
    SB_THUMBPOSITION:
      FVertOffset := Msg.Pos;
  end;
  FVertOffset := Max(0, Min(FMaxVScroll, FVertOffset));
  NormalizeVertOffset;
  if (FVertOffset <> Old) then
  begin
    SetScrollBar(SB_VERT, 0, FVertOffset, SIF_POS);
    InvalidateDownWard(FixedHeight);
  end;
end;

procedure TNiceGrid.SetColCount(Value: Integer);
begin
  if (ColCount <> Value) then
  begin
    FColumns.BeginUpdate;
    while (ColCount > Value)
      do FColumns.Delete(FColumns.Count-1);
    while (ColCount < Value)
      do FColumns.Add;
    FHorzOffset := 0;
    FVertOffset := 0;
    FCol := Max(0, Min(FCol, ColCount-1));
    FRow := Max(0, Min(FRow, FRowCount-1));
    if (FRowCount = 0) or (ColCount = 0) then
    begin
      FCol := -1;
      FRow := -1;
    end;
    FSelectArea := Rect(FCol, FRow, FCol, FRow);
    FColumns.EndUpdate;
    ColRowChanged;
  end;
end;

procedure TNiceGrid.SetRowCount(Value: Integer);
begin
  if (FRowCount <> Value) then
  begin
    FRowCount := Value;
    FCol := Max(0, Min(FCol, ColCount-1));
    FRow := Max(0, Min(FRow, FRowCount-1));
    if (FRowCount = 0) or (ColCount = 0) then
    begin
      FCol := -1;
      FRow := -1;
    end;
    FSelectArea := Rect(FCol, FRow, FCol, FRow);
    Recalculate;
    Invalidate;
    ColRowChanged;
  end;
end;

procedure TNiceGrid.ClearHeaderInfos;
var
  x: Integer;
  P: PHeaderInfo;
begin
  for x := 0 to FHeaderInfos.Count-1 do
  begin
    P := PHeaderInfo(FHeaderInfos[x]);
    Dispose(P);
  end;
  FHeaderInfos.Clear;
end;

procedure TNiceGrid.Recalculate;
var
  x: Integer;
  HVisible, VVisible: Boolean;
  VisCount: Integer;
  WidthAvail, HeightAvail: Integer;
  v: Integer;
  LastBodyWidth: Integer;

  function GetColAutoWidth(i: Integer): Integer;
  var
    n: Integer;
    t: TStrings;
  begin
    Result := 0;
    t := Columns[i].FStrings;
    for n := 0 to t.Count-1
      do Result := Max(Result, Canvas.TextWidth(t[n]) + 7);
    Result := Max(Result, 20);
  end;

begin

  BuildMergeData;

  VisCount := 0;
  for x := 0 to FColumns.Count-1 do
  begin
    if FColumns[x].FVisible
      then Inc(VisCount);
  end;

  if (VisCount = 0) then
  begin
    FixedHeight := 0;
    FixedWidth := 0;
    BodyWidth := 0;
    BodyHeight := 0;
    ShowHideScrollBar(False, False);
    Exit;
  end;

  if FAutoColWidth then
  begin
    Canvas.Font.Assign(Font);
    for x := 0 to FColumns.Count-1
      do FColumns[x].FWidth := Max(FDefColWidth, GetColAutoWidth(x));
  end;

  FixedWidth := 0;
  if (FGutterKind <> gkNone)
    then FixedWidth := FGutterWidth;

  FixedHeight := FHeaderLine * FDefRowHeight;
  BodyHeight  := FRowCount * FDefRowHeight;

  WidthAvail := ClientWidth - FixedWidth;
  HeightAvail := ClientHeight - FixedHeight;
  if FShowFooter
    then HeightAvail := HeightAvail - FDefRowHeight;

  BodyWidth := 0;
  for x := 0 to FColumns.Count-1 do
  begin
    if FColumns[x].FVisible
      then BodyWidth := BodyWidth + FColumns[x].FWidth;
  end;

  if FFitToWidth then
  begin
    if (BodyWidth < WidthAvail) then
    begin
      LastBodyWidth := BodyWidth;
      x := 0;
      while (BodyWidth < WidthAvail) do
      begin
        if (x > ColCount-1) then
        begin
          if (BodyWidth = LastBodyWidth)
            then Break
            else x := 0;
        end;
        if FColumns[x].FVisible and FColumns[x].FCanResize then
        begin
          FColumns[x].FWidth := FColumns[x].FWidth + 1;
          Inc(BodyWidth);
        end;
        Inc(x);
      end;
    end;
    if (BodyWidth > WidthAvail) then
    begin
      LastBodyWidth := BodyWidth;
      x := 0;
      while (BodyWidth > WidthAvail) do
      begin
        if (x > ColCount-1) then
        begin
          if (BodyWidth = LastBodyWidth)
            then Break
            else x := 0;
        end;  
        if FColumns[x].FVisible and (x <> ForcedColumn) and FColumns[x].FCanResize then 
        begin
          FColumns[x].FWidth := FColumns[x].FWidth - 1;
          Dec(BodyWidth);
        end;
        Inc(x);
      end;
    end;
    ForcedColumn := -1;
  end;

  if (BodyWidth < WidthAvail)
    then FHorzOffset := 0;

  if (BodyHeight < HeightAvail)
    then FVertOffset := 0;

  HVisible := BodyWidth > WidthAvail;
  VVisible := BodyHeight > HeightAvail;

  ShowHideScrollBar(HVisible, VVisible);

  FMaxHScroll := Max(0, BodyWidth - ClientWidth + FixedWidth);
  if FShowFooter
    then FMaxVScroll := Max(0, BodyHeight - ClientHeight + FixedHeight + FDefRowHeight)
    else FMaxVScroll := Max(0, BodyHeight - ClientHeight + FixedHeight);

  // Align to FDefRowHeight
  v := FMaxVScroll div FDefRowHeight;
  if (FMaxVScroll mod FDefRowHeight) > 0
    then Inc(v);
  FMaxVScroll := v * FDefRowHeight;

  if FShowFooter then
  begin
    if VVisible
      then FooterTop := (((ClientHeight div FDefRowHeight) - 1) * FDefRowHeight) - 1
      else FooterTop := (FDefRowHeight * (FHeaderLine + FRowCount)) - 1;
  end;

  FHorzOffset := Max(0, Min(FHorzOffset, FMaxHScroll));
  FVertOffset := Max(0, Min(FVertOffset, FMaxVScroll));

  SetScrollBar(SB_HORZ, FMaxHScroll, FHorzOffset, SIF_POS or SIF_RANGE);
  SetScrollBar(SB_VERT, FMaxVScroll, FVertOffset, SIF_POS or SIF_RANGE);

  AllWidth := Min(ClientWidth, BodyWidth + FixedWidth);
  if FShowFooter then
  begin
    AllHeight := Min(ClientHeight, BodyHeight + FixedHeight + FDefRowHeight);
    CellBox := Rect(FixedWidth, FixedHeight, ClientWidth, FooterTop);
  end else
  begin
    AllHeight := Min(ClientHeight, BodyHeight + FixedHeight);
    CellBox := Rect(FixedWidth, FixedHeight, ClientWidth, ClientHeight);
  end;
end;

function DrawString(Canvas: TCanvas; Str: string; Rc: TRect;
  HorzAlign: THorzAlign; VertAlign: TVertAlign; IsEditing: Boolean): TPoint;
var
  w, h, x, y: Integer;
  rw: Integer;
begin
  w := Canvas.TextWidth(Str);
  h := Canvas.TextHeight('gM');
  x := 0;
  y := 0;
  rw := Rc.Right - rc.Left;
  case HorzAlign of
    haLeft:
      begin
        x := Rc.Left;
        if (w > rw) and IsEditing
          then x := Rc.Left - (w - rw);
      end;
    haCenter: x := Rc.Left + ((rw - w) div 2);
    haRight:  x := Rc.Right - w;
  end;
  case VertAlign of
    vaTop:    y := Rc.Top;
    vaCenter: y := Rc.Top + (((Rc.Bottom - Rc.Top) - h) div 2);
    vaBottom: y := Rc.Bottom - h;
  end;
  Canvas.TextRect(Rc, x, y, Str);
  // Return next cursor position
  Result := Point(Min(x + w + 1, Rc.Right), Rc.Top - 1);
end;

procedure DrawStringMulti(Canvas: TCanvas; Str: string; Rc: TRect;
  HorzAlign: THorzAlign; VertAlign: TVertAlign);
var
  w, h, x, y: Integer;
  t: TStringList;
  i: Integer;
  dh: Integer;

begin
  if Pos(';', Str) = 0 then
  begin
    DrawString(Canvas, Str, Rc, HorzAlign, VertAlign, False);
    Exit;
  end;

  t := TStringList.Create;
  t.Text := StringReplace(Str, ';', #13, [rfReplaceAll]);
  h := Canvas.TextHeight('gM');
  dh := Rc.Top + (((Rc.Bottom - Rc.Top) - (h * t.Count)) div 2);
  for i := 0 to t.Count-1 do
  begin
    w := Canvas.TextWidth(t[i]);
    x := 0;
    y := 0;
    case HorzAlign of
      haLeft:   x := Rc.Left;
      haCenter: x := Rc.Left + (((Rc.Right - Rc.Left) - w) div 2);
      haRight:  x := Rc.Right - w;
    end;
    case VertAlign of
      vaTop:    y := Rc.Top + (i * h);
      vaCenter: y := dh + (i * h);
      vaBottom: y := Rc.Bottom - (h * (t.Count-i));
    end;
    Canvas.TextRect(Rc, x, y, t[i]);
  end;
  t.Free;
end;

function TNiceGrid.GetCellColor(X, Y: Integer): TColor;
var
  cl: TColor;
  R: TRect;
begin
  cl := FColumns[x].Color;
  if Odd(Y) then
  begin
    if (cl = FColor)
      then cl := FAlternateColor;
  end;
  if FEnabled then
  begin
    with FSelectArea
      do R := Rect(Left, Top, Right + 1, Bottom + 1);
    if PtInRect(R, Point(X, Y)) then
    begin
      if not ((X = FCol) and (y = FRow))
        then cl := FSelectionColor;
    end;
  end;
  Result := cl;
end;

procedure TNiceGrid.DrawFixCell(Rc: TRect; Str: string; AFont: TFont; AEvent: TOnDrawHeaderEvent);
var
  Rt: TRect;
  Handled: Boolean;
begin
  Handled := False;
  with Canvas do
  begin
    // Clear area
    if FFlat
      then Pen.Color := FHeaderDarkColor
      else Pen.Color := clBlack;
    Brush.Style := bsSolid;
    Brush.Color := FHeaderColor;
    Font.Assign(AFont);
    if not FEnabled
      then Font.Color := FHeaderDarkColor;
    if Assigned(AEvent)
      then AEvent(Self, Canvas, Rc, Str, Handled);
    if Handled
      then Exit;
    Rectangle(Rc);
    // Draw text immediately
    Brush.Style := bsClear;
    Rt := Rect(Rc.Left + 2, Rc.Top + 2, Rc.Right - 3, Rc.Bottom - 3);
    DrawStringMulti(Canvas, Str, Rt, haCenter, vaCenter);
    // cosmetics
    Pen.Color := FHeaderLightColor;
    MoveTo(Rc.Left + 1, Rc.Bottom - 2);
    LineTo(Rc.Left + 1, Rc.Top + 1);
    LineTo(Rc.Right - 1, Rc.Top + 1);
    if not FFlat then
    begin
      Pen.Color := FHeaderDarkColor;
      MoveTo(Rc.Right - 2, Rc.Top + 1);
      LineTo(Rc.Right - 2, Rc.Bottom - 2);
      LineTo(Rc.Left, Rc.Bottom - 2);
    end;
  end;
end;

procedure TNiceGrid.RenderGutter;
const
  ArrowWidth = 8;
var
  x: Integer;
  R, Dummy: TRect;
  Str: string;
  l, t, m: Integer;
  GutterBox: TRect;
begin
  if (FGutterKind = gkNone)
    then Exit;
  CopyRect(GutterBox, CellBox);
  GutterBox.Left := 0;
  for x := 0 to FRowCount-1 do
  begin
    R := Rect(-1, (x * FDefRowHeight) - 1, FGutterWidth, ((x + 1) * FDefRowHeight));
    OffsetRect(R, 0, -FVertOffset + FixedHeight);
    if IntersectRect(Dummy, R, GutterBox) then
    begin
      case FGutterKind of
        gkBlank, gkPointer:
          Str := '';
        gkNumber:
          Str := IntToStr(x + 1);
        gkString:
          if (x > FGutterStrings.Count-1)
            then Str := ''
            else Str := FGutterStrings[x];
      end;
      DrawFixCell(R, Str, FGutterFont, FOnDrawGutter);
      // Draw pointer triangle
      if (FGutterKind = gkpointer) and (x = FRow) then
      begin
        with Canvas do
        begin
          l := (FGutterWidth - ArrowWidth) div 2;
          t := (FDefRowHeight - ArrowWidth) div 2;
          m := R.Top + (FDefRowHeight div 2);
          Pen.Color := FHeaderDarkColor;
          MoveTo(l, R.Bottom - t);
          LineTo(l, R.Top + t);
          LineTo(l + ArrowWidth, m);
          Pen.Color := FHeaderLightColor;
          LineTo(l, R.Bottom - t);
        end;
      end;
    end;
  end;
end;

procedure TNicegrid.RenderHeader;
var
  x: Integer;
  R, Dummy: TRect;
  P: PHeaderInfo;
begin
  Canvas.Font.Assign(FHeaderFont);
  for x := 0 to FHeaderInfos.Count-1 do
  begin
    P := PHeaderInfo(FHeaderInfos[x]);
    R := Rect(
           GetColCoord(P^.Rc.Left) - 1,
           (FDefRowHeight * P^.Rc.Top) - 1,
           GetColCoord(P^.Rc.Right + 1),
           FDefRowHeight * (P^.Rc.Bottom + 1)
         );
    OffsetRect(R, -FHorzOffset + FixedWidth, 0);
    if IntersectRect(Dummy, R, ClientRect)
      then DrawFixCell(R, P^.Str, FHeaderFont, FOnDrawHeader);
  end;
  R := Rect(-1, -1, FixedWidth, FixedHeight);
  DrawFixCell(R, '', FHeaderFont, FOnDrawHeader);
end;

procedure TNiceGrid.RenderFooter;
var
  x: Integer;
  R, Dummy: TRect;
  FooterBottom: Integer;
  Right: Integer;
begin
  Canvas.Font.Assign(FFooterFont);
  FooterBottom := FooterTop + FDefRowHeight + 1;
  for x := 0 to FColumns.Count-1 do
  begin
    R := Rect(GetColCoord(x)-1, FooterTop, GetColCoord(x+1), FooterBottom);
    OffsetRect(R, -FHorzOffset + FixedWidth, 0);
    if IntersectRect(Dummy, R, ClientRect)
      then DrawFixCell(R, FColumns[x].FFooter, FFooterFont, FOnDrawFooter);
  end;
  R := Rect(-1, FooterTop, FixedWidth, FooterBottom);
  DrawFixCell(R, '', FFooterFont, FOnDrawFooter);
  Right := Min(AllWidth, ClientWidth);
  R := Rect(-1, FooterBottom-1, Right, ClientHeight);
  DrawFixCell(R, '', FFooterFont, FOnDrawFooter);
end;

procedure TNiceGrid.DrawCell(X, Y: Integer);
var
  R, Rc, Dummy: TRect;
  Column: TNiceColumn;
  Handled: Boolean;
begin
  Handled := False;
  Rc := GetCellRect(x, y);
  OffsetRect(Rc, -FHorzOffset + FixedWidth, -FVertOffset + FixedHeight);
  R := Rc;
  if IntersectRect(Dummy, Rc, CellBox) then
  begin
    Column := FColumns[x];
    with Canvas do
    begin
      Font.Assign(Column.Font);
      if not FEnabled
        then Font.Color := FGridColor;
      Pen.Color := FGridColor;
      Brush.Color := GetCellColor(X, Y);

      if Assigned(FOnDrawCell)
        then FOnDrawCell(Self, Canvas, X, Y, Rc, Handled);

      if not Handled then
      begin
        Brush.Style := bsSolid;
        if FShowGrid
          then Rectangle(Rc)
          else FillRect(Rc);
        Brush.Style := bsClear;
        InflateRect(Rc, -4, -2);
        DrawString(Canvas, SafeGetCell(x, y), Rc, Column.HorzAlign,
          Column.VertAlign, False);
      end;

    end;
  end;
end;

function TNiceGrid.FastDrawCell(X, Y: Integer; IsEditing: Boolean): TPoint;
var
  R, Dummy: TRect;
  Handled: Boolean;
  Column: TNiceColumn;
begin
  Handled := False;          
  Result := Point(-1, -1);
  R := GetCellRect(x, y);
  OffsetRect(R, -FHorzOffset + FixedWidth, -FVertOffset + FixedHeight);
  if IntersectRect(Dummy, R, CellBox) then
  begin
    Column := FColumns[x];
    with Canvas do
    begin
      Brush.Color := GetCellColor(X, Y);
      Font.Assign(Column.Font);
    end;
    if Assigned(FOnDrawCell)
      then FOnDrawCell(Self, Canvas, X, Y, R, Handled);
    if not Handled then
    begin
      with Canvas do
      begin
        Brush.Style := bsSolid;
        InflateRect(R, -4, -2);
        FillRect(R);
        Brush.Style := bsClear;
      end;
      Result := DrawString(Canvas, SafeGetCell(x, y), R, Column.HorzAlign,
        Column.VertAlign, IsEditing);
    end;
  end;
end;

procedure TNiceGrid.DrawSelection;
var
  R, R1, R2: TRect;
  HOffset, VOffset: Integer;

begin

  if (FCol = -1) or (FRow = -1)
    then Exit;

  HOffset := - FHorzOffset + FixedWidth;
  VOffset := - FVertOffset + FixedHeight;

  R1 := GetCellRect(FSelectArea.Left, FSelectArea.Top);
  R2 := GetCellRect(FSelectArea.Right, FSelectArea.Bottom);
  R := Rect(R1.Left, R1.Top, R2.Right, R2.Bottom);
  OffsetRect(R, HOffset, VOffset);

  with Canvas do
  begin

    if Focused
      then Pen.Color := clBlack
      else Pen.Color := FGridColor;

    Pen.Width := 3;
    Brush.Style := bsClear;
    Rectangle(R);

    Pen.Width := 1;
    Brush.Style := bsSolid;
    if Focused
      then Brush.Color := clBlack
      else Brush.Color := FGridColor;
    Pen.Color := clWhite;

    case SmallBoxPos of
      0: SmallBox := Rect(R.Right - 3, R.Bottom - 3, R.Right + 3, R.Bottom + 3);
      1: SmallBox := Rect(R.Right - 3, R.Top - 3 + 5, R.Right + 3, R.Top + 3 + 5);
      2: SmallBox := Rect(R.Left - 3 + 5, R.Bottom - 3, R.Left + 3 + 5, R.Bottom + 3);
    end;

    Rectangle(SmallBox);
    SmallBoxPos := 0;  // Reset to Right Bottom

  end;

  if (SmallBoxArea.Left <> -1) then
  begin
    R1 := GetCellRect(SmallBoxArea.Left, SmallBoxArea.Top);
    OffsetRect(R1, HOffset, VOffset);
    R2 := GetCellRect(SmallBoxArea.Right, SmallBoxArea.Bottom);
    OffsetRect(R2, HOffset, VOffset);
    R := Rect(R1.Left, R1.Top, R2.Right, R2.Bottom);

    with Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Width := 1;
      Pen.Style := psDot;
      Brush.Style := bsClear;
      Rectangle(R);
      Pen.Style := psSolid;
      Pen.Width := 1;
    end;

  end;

end;

procedure TNiceGrid.ClearUnused;
var
  t: Integer;
begin
  if (AllWidth < ClientWidth) then
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      FillRect(Rect(AllWidth, 0, ClientWidth, ClientHeight));
    end;
  end;
  if FShowFooter
    then Exit;
  if (AllHeight < ClientHeight) then
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      FillRect(Rect(0, AllHeight, ClientWidth, ClientHeight));
    end;
  end;
  if ((FMaxVScroll - FVertOffset) < FDefRowHeight) then
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      t := FixedHeight + (((ClientHeight - FixedHeight) div FDefRowHeight) * FDefRowHeight);
      FillRect(Rect(0, t, ClientWidth, ClientHeight));
    end;
  end;
end;

procedure TNiceGrid.Paint;
var
  x, y: Integer;
  RgnInv, RgnAll, RgnBody, RgnSel, Temp: HRGN;
  HOffset, VOffset: Integer;
  R, R1, R2: TRect;

begin

  if FUpdating then Exit;
  if not HandleAllocated then Exit;

  if (ColCount = 0) then
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      FillRect(Rect(0, 0, ClientWidth, ClientHeight));
    end;
    Exit;
  end;

  if (FRowCount > 0) then
  begin

    // Calculating area that will be covered by selection rectangle
    HOffset := - FHorzOffset + FixedWidth;
    VOffset := - FVertOffset + FixedHeight;
    R1 := GetCellRect(FSelectArea.Left, FSelectArea.Top);
    R2 := GetCellRect(FSelectArea.Right, FSelectArea.Bottom);
    R := Rect(R1.Left, R1.Top, R2.Right, R2.Bottom);
    OffsetRect(R, HOffset, VOffset);

    // Creating region, excluding selection rectangle to reduce flicker
    RgnSel := CreateRectRgn(R.Left-1, R.Top-1, R.Right+1, R.Bottom+1);
    Temp := CreateRectRgn(R.Left+2, R.Top+2, R.Right-2, R.Bottom-2);
    CombineRgn(RgnSel, RgnSel, Temp, RGN_XOR);

    if FShowFooter
      then RgnInv := CreateRectRgn(FixedWidth, FixedHeight, ClientWidth, FooterTop)
      else RgnInv := CreateRectRgn(FixedWidth, FixedHeight, ClientWidth, ClientHeight);
    if FEnabled
      then CombineRgn(RgnInv, RgnInv, RgnSel, RGN_DIFF);
    SelectClipRgn(Canvas.Handle, RgnInv);

    for x := 0 to ColCount-1 do
    begin
      if FColumns[x].FVisible then
      begin
        for y := 0 to FRowCount-1 do
        begin
          if (Integer(GetObject(x, y)) <> MergeID)
            then DrawCell(X, Y);
        end;
      end;           
    end;

    for x := 0 to Mergeds.Count-1
      do DrawMergedCell(x);

    RgnAll := CreateRectRgn(0, 0, ClientWidth, ClientHeight);
    if FEnabled
      then CombineRgn(RgnAll, RgnAll, RgnSel, RGN_DIFF);
    SelectClipRgn(Canvas.Handle, RgnAll);
    ClearUnused;

    if FShowFooter
      then RgnBody := CreateRectRgn(FixedWidth, FixedHeight, ClientWidth, FooterTop)
      else RgnBody := CreateRectRgn(FixedWidth, FixedHeight, ClientWidth, ClientHeight);
    SelectClipRgn(Canvas.Handle, RgnBody);
    if FEnabled
      then DrawSelection;

    SelectClipRgn(Canvas.Handle, 0);

    DeleteObject(RgnInv);
    DeleteObject(RgnAll);
    DeleteObject(RgnBody);
    DeleteObject(RgnSel);
    DeleteObject(Temp);

  end else

    ClearUnused;

  RenderGutter;
  RenderHeader;
  if FShowFooter
    then RenderFooter;

end;

procedure TNiceGrid.UpdateHeader;
var
  P: PHeaderInfo;
  x, y: Integer;
  t: TStringList;
  s: string;
  LastX: TList;
  LastY: PHeaderInfo;
  Blank: PHeaderInfo;

begin

  ClearHeaderInfos;

  LastX := TList.Create;
  t := TStringList.Create;

  Blank := New(PHeaderInfo);
  Blank^.Str := '^%%%%%^******^';

  while (LastX.Count < FHeaderLine)
    do LastX.Add(Blank);

  P := nil;
  for x := 0 to FColumns.Count-1 do
  begin
    if not FColumns[x].FVisible then
    begin
      for y := 0 to FHeaderLine-1
        do LastX[y] := Blank;
      Continue;
    end;
    t.Text := StringReplace(FColumns[x].Title, '|', #13, [rfReplaceAll]);
    while (t.Count < FHeaderLine) do
    begin
      if (t.Count = 0)
        then t.Add('')
        else t.Add( t[t.Count-1]);
    end;
    LastY := Blank;
    for y := 0 to FHeaderLine-1 do
    begin
      s := t[y];
      if (s = LastY.Str) then
      begin
        LastY^.Rc.Bottom := Min(FHeaderLine-1, Max(LastY^.Rc.Bottom, y));
      end else
      begin
        if (s = PHeaderInfo(LastX[y])^.Str) then
        begin
          P := PHeaderInfo(LastX[y]);
          P^.Rc.Right := P^.Rc.Right + 1;
        end else
        begin
          P := New(PHeaderInfo);
          P^.Rc := Rect(x, y, x, y);
          P^.Str := s;
          FHeaderInfos.Add(P);
        end;
        LastX[y] := P;
      end;
      LastY := P;
    end;
  end;

  LastX.Free;
  t.Free;
  Dispose(Blank);

  Recalculate;
end;

function TNiceGrid.GetColCoord(I: Integer): Integer;
var
  x: Integer;
  Column: TNiceColumn;
begin
  Result := 0;
  for x := 0 to I-1 do
  begin
    Column := FColumns[x];
    if Column.FVisible
      then Result := Result + Column.FWidth;
  end;  
end;

function TNiceGrid.GetCellRect(x, y: Integer): TRect;
var
  l, t, w, h: Integer;
begin
  if (x = -1) or (y = -1) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  l := GetColCoord(x);
  t := FDefRowheight * y;
  w := 0;
  if (FColumns[x].FVisible)
    then w := FColumns[x].FWidth;
  h := FDefRowHeight;
  Result := Rect(l-1, t-1, l + w, t + h);
end;

function TNiceGrid.CellRectToClient(R: TRect): TRect;
begin
  Result := R;
  OffsetRect(Result, - FHorzOffset + FixedWidth, - FVertOffset + FixedHeight);
end;

function TNiceGrid.GetCellAtPos(X, Y: Integer): TPoint;
var
  ax, ay: Integer;
begin
  ax := (FHorzOffset + X) - FixedWidth;
  ay := (FVertOffset + Y) - FixedHeight;
  Result.X := 0;
  while (GetColCoord(Result.X) < ax) do
  begin
    Result.X := Result.X + 1;
    if (Result.X > FColumns.Count-1)
      then Break;
  end;
  Result.X := Max(0, Result.X - 1);
  Result.Y := Max(0, Min(ay div FDefRowHeight, FRowCount-1));
end;

function TNiceGrid.GetColFromX(X: Integer): Integer;
var
  ax: Integer;
begin
  if (X < FixedWidth) then
  begin
    Result := -1;
    Exit;
  end;
  Result := 0;
  ax := (FHorzOffset + X) - FixedWidth;
  while (GetColCoord(Result) < ax) do
  begin
    Result := Result + 1;
    if (Result > FColumns.Count-1)
      then Break;
  end;
  Result := Result - 1;
  if (Result > FColumns.Count-1) or (Result < 0)
    then Result := -1;
end;

function TNiceGrid.GetRowFromY(Y: Integer): Integer;
var
  ay: Integer;
begin
  if (Y < FixedHeight) then
  begin
    Result := -1;
    Exit;
  end;
  ay := (FVertOffset + Y) - FixedHeight;
  Result := ay div FDefRowHeight;
  if (Result > FRowCount-1)
    then Result := -1;
end;

function TNiceGrid.SafeGetCell(X, Y: Integer): string;
var
  t: TStringList;
begin
  Result := '';
  t := TStringList(Columns[X].FStrings);
  if (Y < t.Count)
    then Result := t[Y];
end;

function TNiceGrid.GetCell(X, Y: Integer): string;
var
  t: TStrings;
begin
  Result := '';
  if (X > ColCount-1) or (Y > FRowCount-1)
    then raise Exception.Create('Cell Index out of bound.');
  t := Columns[X].FStrings;
  if (Y < t.Count)
    then Result := t[Y];
end;

procedure TNiceGrid.InternalSetCell(X, Y: Integer; Value: string;
  FireOnChange: Boolean);
var
  t: TStringList;
  s: string;
  CanChange: Boolean;
begin
  if (ColCount = 0) or (FRowCount = 0)
    then Exit;
  if FireOnChange and FColumns[X].FReadOnly
    then Exit;
  if (X > ColCount-1) or (Y > FRowCount-1)
    then raise Exception.Create('Cell Index out of bound.');
  t := TStringList(FColumns[X].FStrings);
  while (Y > t.Count-1)
    do t.Add('');
  if (t[Y] = Value)
    then Exit;  
  if FireOnChange then
  begin
    s := Value;
    CanChange := True;
    if Assigned(FOnCellChanging)
      then FOnCellChanging(Self, X, Y, CanChange);
    if not CanChange
      then Exit;
    if Assigned(FOnCellChange)
      then FOnCellChange(Self, X, Y, s);
    t[Y] := s;
  end else
    t[Y] := Value;
  if not FUpdating
    then FastDrawCell(X, Y, False);
end;

procedure TNiceGrid.SetCell(X, Y: Integer; Value: string);
begin
  InternalSetCell(X, Y, Value, False);
end;

procedure TNiceGrid.BeginUpdate;
begin
  FUpdating := True;
  ForceHideCaret;
end;

procedure TNiceGrid.EndUpdate;
begin
  FUpdating := False;
  UpdateHeader;
  Invalidate;
end;

procedure TNiceGrid.SetFlat(Value: Boolean);
begin
  if (FFlat <> Value) then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TNiceGrid.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    inherited Color := Value;
    Invalidate;
  end;
end;

procedure TNiceGrid.SetAlternateColor(Value: TColor);
begin
  if (FAlternateColor <> Value) then
  begin
    FAlternateColor := Value;
    InvalidateCells;
  end;
end;

procedure TNiceGrid.SetGridColor(Value: TColor);
begin
  if (FGridColor <> Value) then
  begin
    FGridColor := Value;
    InvalidateCells;
  end;
end;

function TNiceGrid.GetColWidths(Index: Integer): Integer;
begin
  Result := FColumns[Index].FWidth;
end;

procedure TNiceGrid.SetColWidths(Index, Value: Integer);
begin
  if not FAutoColWidth then
  begin
    if (ColWidths[Index] <> Value)
      then FColumns[Index].Width := Value;
  end;
end;

procedure TNiceGrid.SetAutoColWidth(Value: Boolean);
begin
  if (FAutoColWidth <> Value) then
  begin
    FAutoColWidth := Value;
    Recalculate;
    Invalidate;
  end;
end;

procedure TNiceGrid.SetDefColWidth(Value: Integer);
begin
  if (FDefColWidth <> Value) then
  begin
    FDefColWidth := Value;
    if not FAutoColWidth then
    begin
      Recalculate;
      Invalidate;
    end;
  end;
end;

procedure TNiceGrid.SetDefRowHeight(Value: Integer);
begin
  if (FDefRowHeight <> Value) then
  begin
    FDefRowHeight := Value;
    FSmallChange := Value;
    FLargeChange := Value * 5;
    Recalculate;
    Invalidate;
  end;
end;

procedure TNiceGrid.SetFitToWidth(Value: Boolean);
begin
  if (FFitToWidth <> Value) then
  begin
    FFitToWidth := Value;
    FHorzOffset := 0;
    Recalculate;
    Invalidate;
  end;
end;

procedure TNiceGrid.SetHeaderColor(Value: TColor);
begin
  if (FHeaderColor <> Value) then
  begin
    FHeaderColor := Value;
    Invalidate;
  end;
end;

procedure TNiceGrid.SetHeaderDarkColor(Value: TColor);
begin
  if (FHeaderDarkColor <> Value) then
  begin
    FHeaderDarkColor := Value;
    Invalidate;
  end;
end;

procedure TNiceGrid.SetHeaderLightColor(Value: TColor);
begin
  if (FHeaderLightColor <> Value) then
  begin
    FHeaderLightColor := Value;
    Invalidate;
  end;
end;

procedure TNiceGrid.SetHeaderLine(Value: Integer);
begin
  if (FHeaderLine <> Value) then
  begin
    FHeaderLine := Value;
    UpdateHeader;
    Invalidate;
  end;
end;

procedure TNiceGrid.SetSelectionColor(Value: TColor);
begin
  if (FSelectionColor <> Value) then
  begin
    FSelectionColor := Value;
    InvalidateCells;
  end;
end;

procedure TNiceGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  l, t, r, b: Integer;
  x, y: Integer;
  Empty: Boolean;
  Str: string;
  FillDown: Boolean;
  FillRight: Boolean;
  Old: Integer;
  OldS: string;

  procedure UpdateColRow;
  begin
    ForceHideCaret;
    FUpdating := True;
    BuffString := '';
    FCol2 := FCol;
    FRow2 := FRow;
    EnsureVisible(FCol, FRow);
    FUpdating := False;
    SetSelectArea(Rect(FCol, FRow, FCol, FRow));
    ColRowChanged;
  end;

  procedure UpdateSelectArea;
  begin
    l := Min(FCol2, FCol);
    t := Min(FRow2, FRow);
    r := Max(FCol2, FCol);
    b := Max(FRow2, FRow);
    SetSelectArea(Rect(l, t, r, b));
    EnsureVisible(FCol2, FRow2);
  end;

begin

  if not FEnabled
    then Exit;

  if (ColCount = 0) or (FRowCount = 0)
    then Exit;

  if (ssCtrl in Shift) then
  begin

    case Key of

      Ord('X'), Ord('x'):
        if not FReadOnly
          then CutToClipboard;

      Ord('C'), Ord('c'):
        CopyToClipboard;

      Ord('V'), Ord('v'):
        if not FReadOnly
          then PasteFromClipboard;

      VK_HOME:
        begin
          FCol := GetFirstVisible;
          FRow := 0;
          UpdateColRow;
        end;

      VK_END:
        begin
          FCol := GetLastVisible;
          FRow := FRowCount-1;
          UpdateColRow;
        end;

      VK_DELETE:
        begin
          if not FReadOnly and (FRowCount > 1) then
          begin
            Old := FRow;
            DeleteRow(FRow);
            if Assigned(FOnDeleteRow)
              then FOnDeleteRow(Self, Old);
            UpdateColRow;
          end;
        end;

    end;

  end else

  if (ssShift in Shift) then
  begin

    case Key of

      VK_LEFT:
        begin
          FCol2 := Max(GetPrevVisible(FCol2), GetFirstVisible);
          UpdateSelectArea;
        end;

      VK_RIGHT:
        begin
          FCol2 := Min(GetNextVisible(FCol2), GetLastVisible);
          UpdateSelectArea;
        end;

      VK_UP:
        begin
          FRow2 := Max(FRow2 - 1, 0);
          UpdateSelectArea;
        end;

      VK_DOWN:
        begin
          FRow2 := Min(FRow2 + 1, FRowCount-1);
          UpdateSelectArea;
        end;

      VK_RETURN:
        if (FSelectArea.Left = FSelectArea.Right)
          and (FSelectArea.Top = FSelectArea.Bottom) then
        begin
          FRow := Max(0, FRow - 1);
          UpdateColRow;
        end else
        begin
          if (FCol = FSelectArea.Left) and (FRow = FSelectArea.Top) then
          begin
            FCol := FSelectArea.Right;
            FRow := FSelectArea.Bottom;
          end else
          if (FRow = FSelectArea.Top) then
          begin
            FCol := FCol - 1;
            FRow := FSelectArea.Bottom;
          end else
          begin
            FRow := Row - 1;
          end;
          ForceHideCaret;
          BuffString := '';
          EnsureVisible(FCol, FRow);
          InvalidateCells;
          ColRowChanged;
        end;

    end;

  end else
  begin

    case Key of

      VK_HOME:
        begin
          FCol := GetFirstVisible;
          UpdateColRow;
        end;

      VK_END:
        begin
          FCol := GetLastVisible;
          UpdateColRow;
        end;

      VK_PRIOR:
        begin
          FRow := 0;
          UpdateColRow;
        end;

      VK_NEXT:
        begin
          FRow := FRowCount-1;
          UpdateColRow;
        end;

      VK_LEFT:
        begin
          FCol := Max(GetPrevVisible(FCol), GetFirstVisible);
          UpdateColRow;
        end;

      VK_RIGHT:
        begin
          FCol := Min(GetNextVisible(FCol), GetLastVisible);
          UpdateColRow;
        end;

      VK_UP:
        begin
          if FAutoAddRow and (FRow = (FRowCount-1)) and (FRow > 0) and not FReadOnly then
          begin
            Empty := True;
            for x := 0 to ColCount-1 do
            begin
              if (SafeGetCell(x, FRowCount-1) <> '') then
              begin
                Empty := False;
                Break;
              end;
            end;
            if Empty then
            begin
              RowCount := RowCount - 1;
              FRow := FRowCount - 1;
              if Assigned(FOnDeleteRow)
                then FOnDeleteRow(Self, FRowCount);
            end else
              FRow := Max(0, FRow - 1);
          end else
            FRow := Max(0, FRow - 1);
          UpdateColRow;
        end;

      VK_DOWN:
        begin
          if FAutoAddRow and (FRow = (FRowCount-1)) and not FReadOnly then
          begin
            Inc(FRow);
            RowCount := RowCount + 1;
            if Assigned(FOnInsertRow)
              then FOnInsertRow(Self, FRow);
          end else
            FRow := Min(FRowCount - 1, FRow + 1);
          UpdateColRow;
        end;

      VK_RETURN:
        begin
          OldS := GetCell(Col, Row);
          Str := OldS;

          if Assigned(FOnCellAssignment)
            then FOnCellAssignment(Self, Col, Row, Str);

          if (Str <> Olds)
            then InternalSetCell(Col, Row, Str, True);

          FillDown := FAutoFillDown and (Copy(Str, Length(Str), 1) = '*');
          FillRight := FAutoFillRight and (Copy(Str, Length(Str), 1) = '>');

          if (FSelectArea.Left = FSelectArea.Right)
            and (FSelectArea.Top = FSelectArea.Bottom) then
          begin
            if FillDown then
            begin
              BuffString := '';
              ForceHideCaret;
              Str := Copy(Str, 1, Length(Str) - 1);
              for y := Row to FRowCount-1
                do Cells[Col, y] := Str;
            end else
            if FillRight then
            begin
              BuffString := '';
              ForceHideCaret;
              Str := Copy(Str, 1, Length(Str) - 1);
              for x := Col to ColCount-1
                do Cells[x, Row] := Str;
            end else
            begin
              FRow := Min(FRowCount - 1, FRow + 1);
              UpdateColRow;
            end;
          end else
          begin
            if FillDown then
            begin
              BuffString := '';
              ForceHideCaret;
              Str := Copy(Str, 1, Length(Str) - 1);
              for y := Row to FSelectArea.Bottom
                do Cells[Col, y] := Str;
            end else
            if FillRight then
            begin
              BuffString := '';
              ForceHideCaret;
              Str := Copy(Str, 1, Length(Str) - 1);
              for x := Col to FSelectArea.Right
                do Cells[x, Row] := Str;
            end else
            begin
              if (FCol = FSelectArea.Right) and (FRow = FSelectArea.Bottom) then
              begin
                FCol := FSelectArea.Left;
                FRow := FSelectArea.Top;
              end else
              if (FRow = FSelectArea.Bottom) then
              begin
                FCol := FCol + 1;
                FRow := FSelectArea.Top;
              end else
              begin
                FRow := Row + 1;
              end;
              ForceHideCaret;
              BuffString := '';
              EnsureVisible(FCol, FRow);
              InvalidateCells;
              ColRowChanged;
            end;
          end;
        end;

      VK_DELETE:
        begin
          if (BuffString = '') then
          begin
            if not FReadOnly then
            begin
              FUpdating := True;
              for x := SelectArea.Left to SelectArea.Right do
              begin
                for y := SelectArea.Top to SelectArea.Bottom
                  do InternalSetCell(X, Y, '', True);
              end;
              FUpdating := False;
              InvalidateCells;
            end;
          end;
        end;

      VK_INSERT:
        begin
          if not FReadOnly then
          begin
            InsertRow(Max(0, FRow));
            if Assigned(FOnInsertRow)
              then FOnInsertRow(Self, FRow);
            UpdateColRow;
          end;
        end;

    end;

  end;

  inherited;

end;

procedure TNiceGrid.KeyPress(var Key: Char);
var
  Pt: TPoint;
  Allowed: Boolean;

begin

  inherited;

  if not FEnabled
    then Exit;

  if (ColCount = 0) or (FRowCount = 0)
    then Exit;

  if not FReadOnly then
  begin

    case Key of

      Chr(VK_BACK):
        begin
          ForceHideCaret;
          BuffString := Copy(BuffString, 1, Length(BuffString) - 1);
          InternalSetCell(FCol, FRow, BuffString, True);
          EnsureVisible(FCol, FRow);
          Pt := FastDrawCell(FCol, FRow, True);
          SetCaretPos(Pt.X, Pt.Y);
          ForceShowCaret;
        end;

      Chr($20)..Chr($FF):
        begin
          Allowed := True;
          if Assigned(FOnFilterChar)
            then FOnFilterChar(Self, FCol, FRow, Key, Allowed);
          if Allowed then
          begin
            ForceHideCaret;
            BuffString := BuffString + Key;
            InternalSetCell(FCol, FRow, BuffString, True);
            EnsureVisible(FCol, FRow);
            Pt := FastDrawCell(FCol, FRow, True);
            SetCaretPos(Pt.X, Pt.Y);
            ForceShowCaret;
          end;

        end;

    end;

  end;

end;

function TNiceGrid.GetHitTestInfo(X, Y: Integer): TGridHitTest;
var
  a, i1, i2: Integer;
  ax, ay: Integer;
  IsSizing: Boolean;

begin
  Result := gtNone;
  IsSizing := False;

  ax := (FHorzOffset + X) - FixedWidth;
  ay := (FVertOffset + Y) - FixedHeight;

  if not FAutoColWidth then
  begin
    for a := 1 to ColCount do
    begin
      i1 := GetColCoord(a);
      i2 := X + FHorzOffset - FixedWidth;
      if (i2 > (i1-2)) and (i2 < (i1+2)) then
      begin
        SizingCol := a - 1;
        IsSizing := FColumns[SizingCol].FCanResize;
        Break;
      end;
    end;
  end;

  if PtInRect(SmallBox, Point(X, Y))
    then Result := gtSmallBox else
  if IsSizing
    then Result := gtColSizing else
  if ((X < FixedWidth) and (Y < FixedHeight))
    then Result := gtLeftTop else
  if ((X < FixedWidth) and (Y > FixedHeight) and (ay < BodyHeight))
    then Result := gtLeft else
  if ((Y < FixedHeight) and (X > FixedWidth) and (ax < BodyWidth))
    then Result := gtTop else
  if ((X > FixedWidth) and (Y > FixedHeight) and (ax < BodyWidth) and (ay < BodyHeight))
    then Result := gtCell;

end;

procedure TNiceGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Pt: TPoint;

begin

  if not FEnabled then
  begin
    inherited;
    Exit;
  end;

  if (Cursor = crHSplit) then
  begin
    ForceHideCaret;
    SizingColX := GetColCoord(SizingCol);
  end else

  if (Cursor = crSmallCross) then
  begin
    ForceHideCaret;
    SmallBoxArea := FSelectArea;
  end else

  if (Cursor = crLeftTop) then
  begin
    FRow := 0;
    FCol := 0;
    BuffString := '';
    EnsureVisible(0, 0);
    FCol2 := ColCount-1;
    FRow2 := FRowCount-1;
    SetSelectArea(Rect(0, 0, ColCount-1, FRowCount-1));
    ColRowChanged;
  end else

  if (Cursor = crRight) then
  begin
    FRow := GetRowFromY(Y);
    FCol := 0;
    LastHover := FRow;
    BuffString := '';
    EnsureVisible(FCol, FRow);
    FCol2 := ColCount-1;
    FRow2 := FRow;
    SmallBoxPos := 2;
    AdjustSelection(Rect(0, FRow, ColCount-1, FRow), True);
    ColRowChanged;
    if Assigned(OnGutterClick)
      then FOnGutterClick(Self, FRow, Button, Shift);
  end else

  if (Cursor = crDown) then
  begin
    FCol := GetColFromX(X);
    FRow := 0;
    LastHover := FCol;
    BuffString := '';
    EnsureVisible(FCol, FRow);
    FCol2 := FCol;
    FRow2 := FRowCount-1;
    SmallBoxPos := 1;
    AdjustSelection(Rect(FCol, 0, FCol, FRowCount-1), True);
    ColRowChanged;
    if Assigned(FOnHeaderClick)
      then FOnHeaderClick(Self, FCol, Button, Shift);
  end else

  if (Cursor = crPlus) then
  begin
    BuffString := '';
    Pt := GetCellAtPos(X, Y);
    if (Pt.X = FCol) and (Pt.Y = FRow) then
    begin
      EnsureVisible(FCol, FRow);
      if (not FReadOnly) and (not FColumns[FCol].FReadOnly) then
      begin
        IsEditing := True;
        FEdit.ShowEdit(FCol, FRow);
      end;
    end else
    if (Pt.X <> -1) and (pt.Y <> -1) then
    begin
      EnsureVisible(Pt.X, Pt.Y);
      FCol := Pt.X;
      FRow := Pt.Y;
      BuffString := '';
      FCol2 := FCol;
      FRow2 := FRow;
      SetSelectArea(Rect(FCol, FRow, FCol, FRow));
    end;
    ColRowChanged;
  end;

  SetCapture(Handle);
  SetFocus;

  inherited;

end;

procedure TNiceGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Total2Col: Integer;
  Suggested: Integer;
  Pt: TPoint;
  l, t, r, b: Integer;
  i: Integer;

begin

  if not FEnabled then
  begin
    Cursor := crDefault;
    inherited;
    Exit;
  end;

  if (ssLeft in Shift) then
  begin

    if (Cursor = crPlus) then
    begin
      Pt := GetCellAtPos(X, Y);
      if (Pt.X <> -1) and (Pt.Y <> -1) then
      begin
        l := Min(Pt.X, FCol);
        t := Min(Pt.Y, FRow);
        r := Max(Pt.X, FCol);
        b := Max(Pt.Y, FRow);
        FCol2 := Pt.X;
        FRow2 := Pt.Y;
        SetSelectArea(Rect(l, t, r, b));
        EnsureVisible(FCol2, FRow2);
      end;
    end else

    if (Cursor = crSmallCross) then
    begin
      Pt := GetCellAtPos(X, Y);
      if (Pt.X <> -1) and (Pt.Y <> -1) then
      begin
        l := Min(Pt.X, SmallBoxArea.Left);
        t := Min(Pt.Y, SmallBoxArea.Top);
        r := Max(Pt.X, SmallBoxArea.Right);
        b := Max(Pt.Y, SmallBoxArea.Bottom);
        FCol2 := Pt.X;
        FRow2 := Pt.Y;
        SetSelectArea(Rect(l, t, r, b));
        EnsureVisible(FCol2, FRow2);
      end;
    end else

    if (Cursor = crRight) then
    begin
      i := GetRowFromY(Y);
      if (i <> -1) and (i <> LastHover) then
      begin
        LastHover := i;
        t := Min(i, FRow);
        b := Max(i, FRow);
        FRow2 := i;
        SmallBoxPos := 2;
        AdjustSelection(Rect(0, t, ColCount-1, b), True);
      end;
    end else

    if (Cursor = crDown) then
    begin
      i := GetColFromX(X);
      if (i <> -1) and (i <> LastHover) then
      begin
        LastHover := i;
        l := Min(i, FCol);
        r := Max(i, FCol);
        FCol2 := i;
        SmallBoxPos := 1;
        AdjustSelection(Rect(l, 0, r, FRowCount-1), True);
      end;
    end else

    if (Cursor = crHSplit) then
    begin
      Suggested := Max(5, X + FHorzOffset - SizingColX - FixedWidth);
      if FFitToWidth then
      begin
        if (SizingCol = ColCount-1) or (SizingCol = -1) then
        begin
          inherited;
          Exit;
        end;
        Total2Col := (ClientWidth - FixedWidth) - (TotalWidth - Columns[SizingCol].FWidth - Columns[SizingCol+1].FWidth);
        if (Total2Col > 10) then
        begin
          Columns[SizingCol].FWidth := Suggested;
          Columns[SizingCol+1].FWidth := Total2Col - Suggested;
        end;
        if (Columns[SizingCol+1].FWidth < 5) then
        begin
          Columns[SizingCol].FWidth := Total2Col - 5;
          Columns[SizingCol+1].FWidth := 5;
        end;
      end else
      begin
        Columns[SizingCol].FWidth := Suggested;
      end;
      Recalculate;
      InvalidateRightWard(FixedWidth);
    end;

  end else
  begin

    Cursor := CursorArray[GetHitTestInfo(X, Y)];

  end;

  inherited;

end;

procedure TNiceGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Ls: TList;
  ax, ay: Integer;
  l, t, w, h: Integer;

  function GetCopy(nx, ny: Integer): string;
  var
    ix, iy: Integer;
  begin
    ix := nx;
    iy := ny;
    while (ix < l)
      do ix := ix + w;
    while (iy < t)
      do iy := iy + h;
    ix := ((ix - l) mod w) + l;
    iy := ((iy - t) mod h) + t;
    Result := SafeGetCell(TNiceColumn(Ls[ix]).Index, iy);
  end;

begin

  if (Cursor = crSmallCross) then
  begin
    if FReadOnly then
    begin
      SmallBoxArea := Rect(-1, -1, -1, -1);
      InvalidateCells;
    end else
    begin
      FUpdating := True;
      Ls := TList.Create;
      for ax := FSelectArea.Left to FSelectArea.Right do
        if FColumns[ax].FVisible
          then Ls.Add(FColumns[ax]);
      l := 0;
      for ax := 0 to Ls.Count-1 do
      begin
        if (TNiceColumn(Ls[ax]).Index = SmallBoxArea.Left) then
        begin
          l := ax;
          Break;
        end;
      end;
      t := SmallBoxArea.Top;
      w := (SmallBoxArea.Right - SmallBoxArea.Left) + 1;
      h := (SmallBoxArea.Bottom - SmallBoxArea.Top) + 1;
      for ax := 0 to Ls.Count-1 do
        for ay := FSelectArea.Top to FSelectArea.Bottom
          do InternalSetCell(TNiceColumn(Ls[ax]).Index, ay, GetCopy(ax, ay), True);
      Ls.Free;
      SmallBoxArea := Rect(-1, -1, -1, -1);
      BuffString := '';
      FUpdating := False;
      InvalidateCells;
    end;
  end;

  Cursor := CursorArray[GetHitTestInfo(X, Y)];
  ReleaseCapture;
  LastHover := -1;

  inherited;
end;

procedure TNiceGrid.SetColumns(Value: TNiceColumns);
begin
  FColumns.Assign(Value);
end;

function TNiceGrid.CreateColumn: TNiceColumn;
begin
  Result := TNiceColumn.Create(Columns);
end;

procedure TNiceGrid.UpdateColumn(Index: Integer);
var
  l, w: Integer;
  Rc: TRect;
begin
  l := GetColCoord(Index);
  w := FColumns[Index].FWidth;
  Rc := Rect(l - 3, 0, l + w + 3, ClientHeight);
  InvalidateRect(Handle, @Rc, False);
end;

procedure TNiceGrid.UpdateColumns;
begin
  UpdateHeader;
  Invalidate;
end;

function TNiceGrid.GetColCount: Integer;
begin
  Result := FColumns.Count;
end;

function TNiceGrid.TotalWidth: Integer;
var
  x: Integer;
begin
  Result := 0;
  for x := 0 to FColumns.Count-1 do
  begin
    if FColumns[x].FVisible
      then Result := Result + FColumns[x].FWidth;
  end;
end;

procedure TNiceGrid.CMFontChanged(var Msg: TMessage);
var
  x: Integer;
begin
  inherited;
  for x := 0 to FColumns.Count-1
    do FColumns[x].Font.Assign(Font);
end;

procedure TNiceGrid.WMSize(var Msg: TMessage);
begin
  inherited;
  Recalculate;
  if (FColumns.Count > 0)
    then EnsureVisible(FCol, FRow);
end;

procedure TNiceGrid.WMEraseBkgnd(var Msg: TWMEraseBkGnd);
begin
  Msg.Result := 1;
end;

procedure TNiceGrid.CMWantSpecialKey(var Message: TWMKey);
begin
  inherited;
  with Message do
  case CharCode of
    VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN:
      Result := 1;
  end;
end;

procedure TNiceGrid.SetShowGrid(Value: Boolean);
begin
  if (FShowGrid <> Value) then
  begin
    FShowGrid := Value;
    InvalidateCells;
  end;
end;

procedure TNiceGrid.SetShowFooter(const Value: Boolean);
begin
  if (FShowFooter <> Value) then
  begin
    FShowFooter := Value;
    Recalculate;
    Invalidate;
  end;
end;

procedure TNiceGrid.Clear;
var
  x: Integer;
begin
  for x := 0 to ColCount-1
    do FColumns[x].FStrings.Clear;
  InvalidateCells;  
end;

procedure TNiceGrid.SetHorzOffset(Value: Integer);
begin
  if (FHorzOffset <> Value) then
  begin
    FHorzOffset := Max(0, Min(FMaxHScroll, Value));
    SetScrollBar(SB_HORZ, 0, FHorzOffset, SIF_POS);
    InvalidateRightWard(FixedWidth);
  end;
end;

procedure TNiceGrid.SetVertOffset(Value: Integer);
begin
  if (FVertOffset <> Value) then
  begin
    FVertOffset := Max(0, Min(FMaxVScroll, Value));
    NormalizeVertOffset;
    SetScrollBar(SB_VERT, 0, FVertOffset, SIF_POS);
    InvalidateDownWard(FixedHeight);
  end;
end;

procedure TNiceGrid.EnsureVisible(X, Y: Integer);
var
  t, b, h: Integer;
  l, r: Integer;
  Horz, Vert: Boolean;
  SuggestedHorz, SuggestedVert: Integer;

begin

  if (X = -1) or (Y = -1)
    then Exit;

  if (AllWidth < ClientWidth) and (AllHeight < ClientHeight)
    then Exit;

  SuggestedVert := FVertOffset;
  t := FVertOffset div FDefRowHeight;
  h := ((ClientHeight - FixedHeight) div FDefRowHeight) - 1;
  if FShowFooter
    then h := h-1;
  b := t + h;
  Vert := (Y < t) or (Y > b);
  if (Y < t)
    then SuggestedVert := Y * FDefRowHeight;
  if (Y > b)
    then SuggestedVert := (Y - h) * FDefRowHeight;

  SuggestedHorz := FHorzOffset;
  l := GetColCoord(X) - FHorzOffset + FixedWidth;
  r := l + FColumns[x].FWidth;
  Horz := (l < FixedWidth) or (r > ClientWidth);
  if (l < FixedWidth)
    then SuggestedHorz := Max(0, SuggestedHorz + (l - FixedWidth));
  if (r > ClientWidth)
    then SuggestedHorz := Min(FMaxHScroll, SuggestedHorz - (ClientWidth - r) + 1);

  if Vert and not Horz
    then SetVertOffset(SuggestedVert) else

  if Horz and not Vert
    then SetHorzOffset(SuggestedHorz) else

  if Horz and Vert
    then
    begin
      FHorzOffset := SuggestedHorz;
      FVertOffset := SuggestedVert;
      SetScrollBar(SB_HORZ, 0, FHorzOffset, SIF_POS);
      SetScrollBar(SB_VERT, 0, FVertOffset, SIF_POS);
      Invalidate;
    end;
end;

function TNiceGrid.HeaderCells(I: Integer): THeaderInfo;
begin
  Result := PHeaderInfo(FHeaderInfos[I])^;
end;

function TNiceGrid.HeaderCellsCount: Integer;
begin
  Result := FHeaderInfos.Count;
end;

procedure TNiceGrid.SetReadOnly(Value: Boolean);
begin
  if (FReadOnly <> Value) then
  begin
    FReadOnly := Value;
  end;
end;

procedure TNiceGrid.SetCol(Value: Integer);
begin
  if (FCol <> Value) then
  begin
    ForceHideCaret;
    FCol := Value;
    FCol2 := Value;
    FRow2 := FRow;
    BuffString := '';
    SetSelectArea(Rect(FCol, FRow, FCol, FRow));
    InvalidateRightWard(FixedWidth);
    ColRowChanged;
  end;
end;

procedure TNiceGrid.SetRow(Value: Integer);
begin
  if (FRow <> Value) then
  begin
    ForceHideCaret;
    FRow := Value;
    FRow2 := Value;
    FCol2 := FCol;
    BuffString := '';
    SetSelectArea(Rect(FCol, FRow, FCol, FRow));
    InvalidateDownWard(FixedHeight);
    ColRowChanged;
  end;
end;

procedure TNiceGrid.AdjustSelection(Value: TRect; Force: Boolean);
var
  Old, Rc: TRect;
  R1, R2, R: TRect;
begin

  if EqualRect(FSelectArea, Value) and not Force
    then Exit;

  ForceHideCaret;
  Old := FSelectArea;
  FSelectArea := Value;

  Rc.Left := Min(Old.Left, FSelectArea.Left);
  Rc.Top := Min(Old.Top, FSelectArea.Top);
  Rc.Right := Max(Old.Right, FselectArea.Right);
  Rc.Bottom := Max(Old.Bottom, FSelectArea.Bottom);

  R1 := GetCellRect(Rc.Left, Rc.Top);
  R2 := GetCellRect(Rc.Right, Rc.Bottom);
  R := Rect(R1.Left, R1.Top, R2.Right, R2.Bottom);
  OffsetRect(R, - FHorzOffset + FixedWidth, - FVertOffset + FixedHeight);

  InflateRect(R, 3, 3);
  InvalidateRect(Handle, @R, False);

  if (FGutterKind = gkPointer) then
  begin
    R := Rect(0, FixedHeight, FixedWidth, ClientHeight); 
    InvalidateRect(Handle, @R, False);
  end;

end;

procedure TNiceGrid.SetSelectArea(Value: TRect);
begin
  AdjustSelection(Value, False);
end;


var
  CaretVisible: Boolean = False;

  // I don't think MS's HideCaret and ShowCaret mechanism was a good idea.
  
procedure TNiceGrid.ForceHideCaret;
begin
  if CaretVisible
    then HideCaret(Handle);
  CaretVisible := False;
  FEdit.HideEdit;
end;

procedure TNiceGrid.ForceShowCaret;
begin
  if not CaretVisible
    then ShowCaret(Handle);
  CaretVisible := True;  
end;

procedure TNiceGrid.WMKillFocus(var Msg: TWMKillFocus);
begin
  if (Msg.FocusedWnd <> FEdit.Handle)
    then ForceHideCaret;
  DestroyCaret;
  CaretVisible := False;
  if not IsEditing
    then InvalidateCells;
end;

procedure TNiceGrid.WMSetFocus(var Msg: TWMSetFocus);
begin
  CreateCaret(Handle, 0, 1, FDefRowHeight - 2);
  CaretVisible := False;
  InvalidateCells;
end;

procedure TNiceGrid.SetGutterKind(Value: TGutterKind);
var
  Rc: TRect;
  RedrawAll: Boolean;
  Old: TGutterKind;
begin
  Old := FGutterKind;
  if (FGutterKind <> Value) then
  begin
    FGutterKind := Value;
    Recalculate;
    RedrawAll := (Old = gkNone) or (Value = gkNone);
    if RedrawAll then
    begin
      Invalidate;
    end else
    begin
      Rc := Rect(0, FixedHeight, FixedWidth, ClientHeight);
      InvalidateRect(Handle, @Rc, False);
    end;  
  end;
end;

procedure TNiceGrid.SetGutterWidth(Value: Integer);
begin
  if (FGutterWidth <> Value) then
  begin
    FGutterWidth := Value;
    Recalculate;
    Invalidate;
  end;
end;

procedure TNiceGrid.CopyToClipboard;
var
  s: string;
  t: TStringList;
  x, y: Integer;
begin
  t := TStringList.Create;
  for y := FSelectArea.Top to FSelectArea.Bottom do
  begin
    s := '';
    for x := FSelectArea.Left to FSelectArea.Right do
    begin
      if FColumns[x].FVisible then
      begin
        if (x = FSelectArea.Left)
          then s := SafeGetCell(X, Y)
          else s := s + #9 + SafeGetCell(X, Y);
      end;
    end;
    t.Add(s);
  end;
  Clipboard.AsText := t.Text;
  t.Free;
end;

procedure TNiceGrid.CutToClipboard;
var
  s: string;
  t: TStringList;
  x, y: Integer;
begin
  FUpdating := True;
  t := TStringList.Create;
  for y := FSelectArea.Top to FSelectArea.Bottom do
  begin
    s := '';
    for x := FSelectArea.Left to FSelectArea.Right do
    begin
      if FColumns[x].FVisible then
      begin
        if (x = FSelectArea.Left)
          then s := SafeGetCell(X, Y)
          else s := s + #9 + SafeGetCell(X, Y);
        InternalSetCell(X, Y, '', True);
      end;
    end;
    t.Add(s);
  end;
  Clipboard.AsText := t.Text;
  t.Free;
  FUpdating := False;
  InvalidateCells;
end;

procedure TNiceGrid.PasteFromClipboard;
var
  tr, tc: TStringList;
  x, y: Integer;
  s: string;
  n: Integer;
  TabCnt: Integer;
  ax, ay: Integer;
  ColCnt: Integer;

begin

  if not Clipboard.HasFormat(CF_TEXT)
    then Exit;

  ForceHideCaret;

  FUpdating := True;  
  tr := TStringList.Create;
  tc := TStringList.Create;
  tr.Text := Clipboard.AsText;
  TabCnt := 1;

  for y := 0 to tr.Count-1 do
  begin
    n := 1;
    s := tr[y];
    for x := 1 to Length(s) do
      if (s[x] = #9)
        then Inc(n);
    TabCnt := Max(TabCnt, n);
  end;

  ColCnt := ColCount; // Just to make it fast

  if (FSelectArea.Left = FSelectArea.Right) and (FSelectArea.Top = FSelectArea.Bottom) then
  begin

    for y := 0 to tr.Count-1 do
    begin
      tc.Text := StringReplace(tr[y], #9, #13#10, [rfReplaceAll]);
      while (tc.Count < TabCnt)
        do tc.Add('');
      x := 0;
      ax := FCol;
      while (x < tc.Count) do
      begin
        ay := FRow + y;
        if FColumns[ax].FVisible then
        begin
          if (ax < ColCnt) and (ay < FRowCount)
            then InternalSetCell(ax, ay, tc[x], True);
          Inc(x);
        end;
        Inc(ax);
      end;
    end;

  end else
  begin

    ay := FSelectArea.Top;
    while (ay <= FSelectArea.Bottom) do
    begin
      tc.Text := StringReplace(tr[(ay - FSelectArea.Top) mod tr.Count], #9, #13#10, [rfReplaceAll]);
      while (tc.Count < TabCnt)
        do tc.Add('');
      ax := FSelectArea.Left;
      x := 0;
      while (ax <= FSelectArea.Right) do
      begin
        if FColumns[ax].FVisible then
        begin
          InternalSetCell(ax, ay, tc[x], True);
          Inc(x);
          if (x = tc.Count)
            then x := 0;
        end;
        Inc(ax);
      end;
      Inc(ay);
    end;

  end;

  tr.Free;
  tc.Free;

  FUpdating := False;
  InvalidateCells;

end;

procedure TNiceGrid.InvalidateCells;
var
  Rc: TRect;
begin
  Rc := Rect(FixedWidth-2, FixedHeight-2, ClientWidth, ClientHeight);
  InvalidateRect(Handle, @Rc, False);
end;

procedure TNiceGrid.InvalidateDownWard(Top: Integer);
var
  Rc: TRect;
begin
  Rc := Rect(0, Top, ClientWidth, ClientHeight);
  InvalidateRect(Handle, @Rc, False);
end;

procedure TNiceGrid.InvalidateRightWard(Left: Integer);
var
  Rc: TRect;
begin
  Rc := Rect(Left, 0, ClientWidth, ClientHeight);
  InvalidateRect(Handle, @Rc, False);
end;

procedure TNiceGrid.NormalizeVertOffset;
begin
  FVertOffset := (FVertOffset div FDefRowHeight) * FDefRowHeight;
end;

procedure TNiceGrid.SetGutterFont(Value: TFont);
begin
  FGutterFont.Assign(Value);
  InvalidateGutter;
end;

procedure TNiceGrid.SetHeaderFont(Value: TFont);
begin
  FHeaderFont.Assign(Value);
  InvalidateHeader;
end;

procedure TNiceGrid.SetFooterFont(const Value: TFont);
begin
  FFooterFont.Assign(Value);
  Invalidate;
end;

procedure TNiceGrid.InvalidateGutter;
var
  Rc: TRect;
begin
  Rc := Rect(0, FixedHeight, FixedWidth, ClientHeight);
  InvalidateRect(Handle, @Rc, False);
end;

procedure TNiceGrid.InvalidateHeader;
var
  Rc: TRect;
begin
  Rc := Rect(0, 0, ClientWidth, FixedHeight);
  InvalidateRect(Handle, @Rc, False);
end;

procedure TNiceGrid.HeaderFontChange(Sender: TObject);
begin
  InvalidateHeader;
end;

procedure TNiceGrid.GutterFontChange(Sender: TObject);
begin
  InvalidateGutter;
end;

procedure TNiceGrid.FooterFontChange(Sender: TObject);
begin
  Invalidate;
end;

function TNiceGrid.GetFirstVisible: Integer;
var
  x: Integer;
begin
  Result := -1;
  if (ColCount > 0) then
  begin
    for x := 0 to ColCount-1 do
    begin
      if Columns[x].Visible then
      begin
        Result := x;
        Break;
      end;
    end;
  end;
end;

function TNiceGrid.GetLastVisible: Integer;
var
  x: Integer;
begin
  Result := -1;
  if (ColCount > 0) then
  begin
    for x := ColCount-1 downto 0 do
    begin
      if Columns[x].Visible then
      begin
        Result := x;
        Break;
      end;
    end;
  end;
end;

function TNiceGrid.GetNextVisible(Index: Integer): Integer;
var
  x: Integer;
begin
  Result := Index;
  if (ColCount > 0) and (Index < ColCount) then
  begin
    for x := (Index + 1) to (ColCount - 1) do
    begin
      if Columns[x].Visible then
      begin
        Result := x;
        Break;
      end;
    end;
  end;
end;

function TNiceGrid.GetPrevVisible(Index: Integer): Integer;
var
  x: Integer;
begin
  Result := Index;
  if (ColCount > 0) and (Index > 0) then
  begin
    for x := (Index - 1) downto 0 do
    begin
      if Columns[x].Visible then
      begin
        Result := x;
        Break;
      end;
    end;
  end;
end;

procedure TNiceGrid.DeleteRow(ARow: Integer);
var
  x, y: Integer;
begin
  ForceHideCaret;
  if (ARow >= 0) and (ARow < FRowCount) then
  begin
    for x := 0 to ColCount-1 do
    begin
      with FColumns[x].Strings do
      begin
        if (Count > ARow) then
        begin
          for y := ARow to Count-2
            do Strings[y] := Strings[y + 1];
          Strings[Count-1] := '';
        end;
      end;
    end;
    if (FRow = FRowCount-1)
      then Dec(FRow);
    RowCount := RowCount - 1;
  end;
end;

procedure TNiceGrid.InsertRow(ARow: Integer);
var
  x: Integer;
begin
  ForceHideCaret;
  if (ARow >= 0) and (ARow < FRowCount) then
  begin
    for x := 0 to ColCount-1 do
    begin
      with FColumns[x].Strings do
      begin
        while (Count < ARow)
          do Add('');
        Insert(ARow, '');
      end;
    end;
    RowCount := RowCount + 1;
  end;
end;

function TNiceGrid.AddRow: Integer;
var
  x: Integer;
  n: Integer;
begin
  ForceHideCaret;
  n := FRowCount + 1;
  for x := 0 to ColCount-1 do
  begin
    with FColumns[x].Strings do
    begin
      while (Count < n)
        do Add('');
      Strings[FRowCount] := '';
    end;
  end;
  RowCount := RowCount + 1;
  Result := FRowCount-1;
end;


// This is a workaround to avoid mess up with accelerators.
// NiceGrid was unable to capture keyboard event of chars that already
// defined as accelerator of another control.
// (Char after '&' (ampersand) in ex. TButton.Caption, TMenuItem.Caption, etc.)
// Don't know why and how this workaround works, but this is found after
// spying with WinSight. WM_USER + $B902          - mPri-

procedure TNiceGrid.WMUnknown(var Msg: TMessage);
begin
  Msg.Result := 0;
end;

procedure TNiceGrid.WMMouseWheel(var Msg: TWMMouseWheel);
var
  Old: Integer;
begin
  Old := FVertOffset;
  FVertOffset := Max(0, Min(FMaxVScroll, FVertOffset - Msg.WheelDelta));
  if (FVertOffset <> Old) then
  begin
    SetScrollBar(SB_VERT, 0, FVertOffset, SIF_POS);
    Invalidate;
  end;  
end;

procedure TNiceGrid.ColRowChanged;
begin
  if Assigned(Sync)
    then Sync.Row := FRow;
  if Assigned(FOnColRowChanged)
    then FOnColRowChanged(Self, FCol, FRow);
end;

procedure TNiceGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = Sync) and (Operation = opRemove) 
    then Sync := nil;
  inherited;
end;

procedure TNiceGrid.SetGutterStrings(const Value: TStrings);
begin
  FGutterStrings.Assign(Value);
  if (FGutterKind = gkString)
    then InvalidateGutter;
end;

function TNiceGrid.GetObject(X, Y: Integer): TObject;
var
  t: TStrings;
begin
  Result := nil;
  if (X > ColCount-1) or (Y > FRowCount-1)
    then raise Exception.Create('Cell Index out of bound.');
  t := Columns[X].FStrings;
  if (Y < t.Count)
    then Result := t.Objects[Y];
end;

procedure TNiceGrid.SetObject(X, Y: Integer; const Value: TObject);
var
  t: TStrings;
begin
  if (X > ColCount-1) or (Y > FRowCount-1)
    then raise Exception.Create('Cell Index out of bound.');
  t := Columns[X].FStrings;
  while (Y > t.Count-1)
    do t.Add('');
  t.Objects[Y] := Value;
end;

procedure TNiceGrid.ClearMergeCells;
var
  x, y: Integer;
  List: TStrings;
begin
  for x := 0 to FColumns.Count-1 do
  begin
    List := FColumns[x].FStrings;
    for y := 0 to List.Count-1
      do List.Objects[y] := nil;
  end;
  for x := 0 to Mergeds.Count-1
    do TMergeCell(Mergeds[x]).Free;
  Mergeds.Clear;  
end;

function TNiceGrid.MergeCells(const X1, Y1, X2, Y2: Integer;
  ACaption: string): TMergeCell;
begin
  Result := TMergeCell.Create;
  Result.Font.Assign(Font);
  Result.Color := Color;
  Result.Caption := ACaption;
  Result.HorzAlign := haCenter;
  Result.VertAlign := vaCenter;
  Result.Rc := Rect(Min(X1, X2), Min(Y1, Y2), Max(X1, X2), Max(Y1, Y2));
  Mergeds.Add(Result);
  if not FUpdating then
  begin
    Recalculate;
    Invalidate;
  end;
end;

procedure TNiceGrid.BuildMergeData;
var
  Rc: TRect;
  x, y, z: Integer;
begin
  for x := 0 to Mergeds.Count-1 do
  begin
    CopyRect(Rc, TMergeCell(Mergeds[x]).Rc);
    for y := Rc.Left to Rc.Right do
    begin
      if (y >= FColumns.Count)
        then Continue;
      for z := Rc.Top to Rc.Bottom do
      begin
        InternalSetCell(y, z, '', False);
        SetObject(y, z, TObject(MergeID));
      end;
    end;
  end;
end;

procedure TNiceGrid.DrawMergedCell(Index: Integer);
var
  Data: TMergeCell;
  R, Rc, Dummy: TRect;
  l1, l2, t, h: Integer;
begin
  Data := TMergeCell(Mergeds[Index]);
  l1 := GetColCoord(Data.Rc.Left);
  l2 := GetColCoord(Data.Rc.Right + 1);
  t := FDefRowHeight * Data.Rc.Top;
  h := FDefRowHeight * (Data.Rc.Bottom - Data.Rc.Top + 1);
  Rc := Rect(l1-1, t-1, l2, t+h);
  OffsetRect(Rc, -FHorzOffset + FixedWidth, -FVertOffset + FixedHeight);
  R := Rc;
  if IntersectRect(Dummy, Rc, CellBox) then
  begin
    with Canvas do
    begin
      Font.Assign(Data.Font);
      if not FEnabled
        then Font.Color := FGridColor;
      Pen.Color := FGridColor;
      Brush.Color := Data.Color;
      Brush.Style := bsSolid;
      if FShowGrid
        then Rectangle(Rc)
        else FillRect(Rc);
      Brush.Style := bsClear;
      InflateRect(Rc, -4, -2);
      DrawString(Canvas, Data.Caption, Rc, Data.HorzAlign, Data.VertAlign, False);
    end;
  end;
end;

function TNiceGrid.GetHeaderInfo: TList;
begin
  Result := FHeaderInfos;
end;

function TNiceGrid.GetMergedCellsData: TList;
begin
  Result := Mergeds;
end;

procedure TNiceGrid.SetEnabled(const Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    Invalidate;
  end;  
end;

{ TNiceColumn }

constructor TNiceColumn.Create(Collection: TCollection);
begin
  FStrings := TStringList.Create;
  FFont  := TFont.Create;
  FHorzAlign := haLeft;
  FVertAlign := vaCenter;
  FVisible := True;
  FCanResize := True;
  FReadOnly := False;
  FTag := 0;
  FTag2 := 0;
  with TNiceColumns(Collection).Grid do
  begin
    Self.FFont.Assign(Font);
    Self.FWidth := DefColWidth;
    Self.FColor := Color;
  end;
  FFont.OnChange := FontChange;
  inherited Create(Collection);
end;

destructor TNiceColumn.Destroy;
begin
  inherited Destroy;
  FFont.Free;
  FStrings.Free;
end;

procedure TNiceColumn.Assign(Source: TPersistent);
begin
  if (Source is TNiceColumn) then
  begin
    Title     := TNiceColumn(Source).Title;
    Footer    := TNiceColumn(Source).Footer;
    Width     := TNiceColumn(Source).Width;
    Font      := TNiceColumn(Source).Font;
    Color     := TNiceColumn(Source).Color;
    HorzAlign := TNiceColumn(Source).HorzAlign;
    VertAlign := TNiceColumn(Source).VertAlign;
    Visible   := TNiceColumn(Source).Visible;
    Tag       := TNiceColumn(Source).Tag;
    Tag2      := TNiceColumn(Source).Tag2;
    Hint      := TNiceColumn(Source).Hint;
    CanResize := TNiceColumn(Source).CanResize;
    ReadOnly  := TNiceColumn(Source).ReadOnly;
    Strings.Assign(TNiceColumn(Source).Strings);
    Changed(False);
  end;
end;

procedure TNiceColumn.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

procedure TNiceColumn.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  Changed(False);
end;

procedure TNiceColumn.SetHorzAlign(Value: THorzAlign);
begin
  if (FHorzAlign <> Value) then
  begin
    FHorzAlign := Value;
    Changed(False);
  end;
end;

procedure TNiceColumn.SetTitle(Value: string);
begin
  if (FTitle <> Value) then
  begin
    FTitle := Value;
    Changed(True);
  end;
end;

procedure TNiceColumn.SetFooter(const Value: string);
begin
  if (FFooter <> Value) then
  begin
    FFooter := Value;
    Changed(False);
  end;
end;

procedure TNiceColumn.SetVertAlign(Value: TVertAlign);
begin
  if (FVertAlign <> Value) then
  begin
    FVertAlign := Value;
    Changed(False);
  end;
end;

procedure TNiceColumn.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed(True);
  end;
end;

procedure TNiceColumn.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    TNiceColumns(Collection).FGrid.ForcedColumn := Index;
    Changed(True);
  end;
end;

procedure TNiceColumn.SetStrings(Value: TStrings);
begin
  FStrings.Assign(Value);
  Changed(False);
end;

procedure TNiceColumn.FontChange(Sender: TObject);
begin
  Changed(False);
end;

function TNiceColumn.IsFontStored: Boolean;
begin
  Result := True;
  with TNiceColumns(Collection).FGrid.Font do
  begin
    if (Charset = FFont.Charset) and
      (Color = FFont.Color) and
      (Height = FFont.Height) and
      (Name = FFont.Name) and
      (Pitch = FFont.Pitch) and
      (PixelsPerInch = FFont.PixelsPerInch) and
      (Size = FFont.Size) and
      (Style = FFont.Style)
         then Result := False;
  end;
end;

function TNiceColumn.GetGrid: TNiceGrid;
begin
  Result := TNiceColumns(Collection).FGrid;
end;

function TNiceColumn.GetDisplayName: string;
begin
  if (FTitle <> '')
    then Result := FTitle
    else Result := 'Column ' + IntToStr(Index);
end;

{ TNiceColumns }

constructor TNiceColumns.Create(AGrid: TNiceGrid);
begin
  inherited Create(TNiceColumn);
  FGrid := AGrid;
end;

function TNiceColumns.Add: TNiceColumn;
begin
  Result := TNiceColumn(inherited Add);
end;

function TNiceColumns.GetItem(Index: Integer): TNiceColumn;
begin
  Result := TNiceColumn(inherited GetItem(Index));
end;

procedure TNiceColumns.SetItem(Index: Integer; Value: TNiceColumn);
begin
  inherited SetItem(Index, Value);
end;

function TNiceColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

function TNiceColumns.Insert(Index: Integer): TNiceColumn;
begin
  Result := AddItem(nil, Index);
end;

function TNiceColumns.AddItem(Item: TNiceColumn;
  Index: Integer): TNiceColumn;
begin
  if (Item = nil)
    then Result := FGrid.CreateColumn
    else
    begin
      Result := Item;
      if Assigned(Item) then
      begin
        Result.Collection := Self;
        if (Index < 0)
          then Index := Count - 1;
        Result.Index := Index;
      end;
    end;
end;

procedure TNiceColumns.Update(Item: TCollectionItem);
begin
  if (Item <> nil)
    then FGrid.UpdateColumn(Item.Index)
    else FGrid.UpdateColumns;
end;


{ TAlignedEdit }

constructor TNiceInplace.Create(Grid: TNiceGrid);
begin
  inherited Create(FGrid);
  FGrid := Grid;
  FAlignment := haLeft;
  Parent := FGrid;
  ParentColor := False;
  BorderStyle := bsNone;
  Left := -200;
  Top := -200;
  Visible := False;
end;

procedure TNiceInplace.CreateParams(var Params: TCreateParams);
const
  Alignments: array [THorzAlign] of Cardinal = (ES_LEFT, ES_CENTER, ES_RIGHT);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or Alignments[FAlignment];
end;

procedure TNiceInplace.SetAlignment(Value: THorzAlign);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TNiceInplace.ShowEdit(X, Y: Integer);
var
  Rc: TRect;
  Column: TNiceColumn;
  l, t, w, h: Integer;
begin

  if CaretVisible
    then HideCaret(Handle);
  CaretVisible := False;

  CellX := X;
  CellY := Y;
  Column := FGrid.FColumns[x];
  Color := FGrid.GetCellColor(X, Y);
  SetAlignment(Column.FHorzAlign);
  Text := FGrid.SafeGetCell(X, Y);
  Font.Assign(Column.FFont);

  Rc := FGrid.GetCellRect(X, Y);
  Rc := FGrid.CellRectToClient(Rc);

  if (FAlignment = haRight)
    then Rc.Right := Rc.Right + 1;
  InflateRect(Rc, -4, -3);

  l := Rc.Left;
  w := Rc.Right - Rc.Left;
  t := 0;
  h := FGrid.Canvas.TextHeight('gM');
  case Column.FVertAlign of
    vaTop:    t := Rc.Top - 1;
    vaCenter: t := Rc.Top + (((Rc.Bottom - Rc.Top) - h) div 2);
    vaBottom: t := Rc.Bottom - h + 1;
  end;

  SetBounds(l, t, w, h);
  Show;

end;

procedure TNiceInplace.HideEdit;
begin
  if Visible
    then Hide;
  FGrid.IsEditing := False;  
end;

procedure TNiceInplace.Change;
begin
  inherited;
  FGrid.InternalSetCell(CellX, CellY, Text, True);
end;

procedure TNiceInplace.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE, VK_RETURN, VK_UP, VK_DOWN:
    begin
      HideEdit;
      FGrid.SetFocus;
    end;
  else
    inherited;
  end;
end;

procedure TNiceInplace.KeyPress(var Key: Char);
var
  Allowed: Boolean;
begin
  Allowed := True;
  if Assigned(FGrid.FOnFilterChar)
    then FGrid.FOnFilterChar(Self, CellX, CellY, Key, Allowed);
  if (not Allowed) and (Key <> Chr(VK_BACK))
    then Key := Chr(0);
  inherited;
end;

{ TNiceGridSync }

constructor TNiceGridSync.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnDeleteRow := SyncDeleteRow;
  FOnInsertRow := SyncInsertRow;
  FOnColRowChanged := SyncColRow;
end;

procedure TNiceGridSync.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FGrid) and (Operation = opRemove)
    then FGrid := nil;
  inherited;
end;

procedure TNiceGridSync.SetGrid(const Value: TNiceGrid);
begin
  if (FGrid <> Value) then
  begin
    FGrid := Value;
    FGrid.Sync := Self;
    FGrid.RowCount := RowCount;
  end;
end;

procedure TNiceGridSync.SetScrollBar(AKind, AMax, APos, AMask: Integer);
begin
  if (AKind = SB_VERT) and Assigned(FGrid) then
  begin
    if ((AMask and SIF_POS) <> 0)
      then FGrid.VertOffset := APos;
  end;
end;

procedure TNiceGridSync.ShowHideScrollBar(HorzVisible,
  VertVisible: Boolean);
begin
  ShowScrollBar(Handle, SB_HORZ, True);
  ShowScrollBar(Handle, SB_VERT, False);
  EnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_BOTH);
end;

procedure TNiceGridSync.SyncColRow(Sender: TObject; ACol, ARow: Integer);
begin
  if Assigned(FGrid)
    then FGrid.Row := ARow;
end;

procedure TNiceGridSync.SyncDeleteRow(Sender: TObject; ARow: Integer);
begin
  if Assigned(FGrid)
    then FGrid.DeleteRow(ARow);
end;

procedure TNiceGridSync.SyncInsertRow(Sender: TObject; ARow: Integer);
begin
  if Assigned(FGrid) then
  begin
    if (ARow = FGrid.RowCount)
      then FGrid.AddRow
      else FGrid.InsertRow(ARow);
  end;
end;

{ TMergeCell }

constructor TMergeCell.Create;
begin
  inherited Create;
  Font := TFont.Create;
end;

destructor TMergeCell.Destroy;
begin
  Font.Free;
  inherited Destroy;
end;

end.
