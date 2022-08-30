{***************************************************************************}
{ TAdvGridWorkbook component                                                }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2003 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvGridWorkbook;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Controls, ExtCtrls, Tabs, StdCtrls, AdvGrid, BaseGrid,
  Grids, Dialogs, Forms, Messages, Graphics, SysUtils, AdvObj
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes, System.Types
  {$ENDIF}
  ;

const
  MAJ_VER = 3; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 2; // Build nr.
  DATE_VER = 'Jul, 2015'; // Month version

  // 2.8.8.3 : Fixed issue with streaming non visible workbook component
  // 3.3.0.0 : Release compatible with latest TAdvStringGrid
  // 3.3.0.1 : Fix for reparenting
  // 3.3.0.2 : Fixed issue with accessing invisible AdvGridWorkbook
  // 3.3.0.3 : Improved : activesheet automatically set correct when using Sheets.Add
  // 3.3.0.4 : Improved : issue with setting active sheet row for empty grids
  // 3.3.0.5 : Fixed : issue with adding sheets with grid with 0 normal rows
  // 3.3.1.0 : New : virtual function GetGridClass: added
  // 3.3.2.0 : New : OnTabSheetChanged event added
  // 3.3.2.1 : Fixed : Issue with switching tabs and grid cells with graphic objects
  // 3.3.2.2 : Fixed : Issue with import of multisheet XLS files with images

type
  TSheetChangeEvent = procedure(Sender: TObject; NewSheet: Integer; var AllowChange: Boolean) of object;

  TSheetChangedEvent = procedure(Sender: TObject; NewSheet: Integer) of object;

  TAdvStringGridClass = class of TAdvStringGrid;

  TAdvGridWorkbook = class;

  TTabLook = class(TPersistent)
  private
    FDitherBackground: Boolean;
    FSelectColor: TColor;
    FUnSelectColor: TColor;
    FBackgroundColor: TColor;
    FFont: TFont;
    FOnChange: TNotifyEvent;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetDitherBackground(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetSelectColor(const Value: TColor);
    procedure SetUnSelectColor(const Value: TColor);
  protected
    procedure FontChanged(Sender: TObject);
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Font: TFont read FFont write SetFont;
    property SelectColor: TColor read FSelectColor write SetSelectColor default clBtnFace;
    property UnSelectColor: TColor read FUnSelectColor write SetUnSelectColor default clWhite;
    property DitherBackground: Boolean read FDitherBackground write SetDitherBackground default true;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGridSheet = class(TCollectionItem)
  private
    FData: TMemoryStream;
    FTag: Integer;
    FName: string;
    FCol: integer;
    FRow: integer;
    FLeftCol: integer;
    FTopRow: integer;
    FColWidths: TIntList;
    FRowHeights: TIntList;
    FSortColumn: Integer;
    procedure SetName(const Value: string);
  protected
    procedure Changed;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Data: TMemoryStream read FData;
    property Col: integer read FCol write FCol;
    property Row: integer read FRow write FRow;
    property TopRow: integer read FTopRow write FTopRow;
    property LeftCol: integer read FLeftCol write FLeftCol;
    property ColWidths: TIntList read FColWidths;
    property RowHeights: TIntList read FRowHeights;
    property SortColumn: Integer read FSortColumn write FSortColumn;
  published
    property Name: string read FName write SetName;
    property Tag: Integer read FTag write FTag;
  end;

  TGridSheetCollection = class(TCollection)
  private
    FOwner: TAdvGridWorkBook;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TGridSheet;
    procedure SetItem(Index: Integer; const Value: TGridSheet);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TAdvGridWorkbook);
    procedure Clear;
    function Add: TGridSheet;
    function Insert(Index: Integer): TGridSheet;
    property Items[Index: Integer]: TGridSheet read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGridWorkbook = class(TCustomControl)
  private
    FTabSet: TTabSet;
    FGrid: TAdvStringGrid;
    FActiveSheet: Integer;
    FAutoCreated: Boolean;
    FWinCreated: Boolean;
    FSheets: TGridSheetCollection;
    FDefaultColCount: Integer;
    FDefaultRowCount: Integer;
    FDefaultRowHeight: Integer;
    FDefaultColWidth: Integer;
    FOnSheetChange: TSheetChangeEvent;
    FOnSheetChanged: TSheetChangedEvent;
    FTabLook: TTabLook;
    FProgSheetChange: boolean;
    procedure NCPaintProc;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure TabChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
    procedure SheetChange(Sender: TObject);
    procedure TabLookChange(Sender: TObject);
    function GetSheets: TGridSheetCollection;
    procedure SetSheets(const Value: TGridSheetCollection);
    procedure SetActiveSheet(const Value: Integer);
    procedure StoreActiveSheet;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetGrid: TAdvStringGrid;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure SheetsChanged(Sender: TObject); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure RestoreSelection(Col,Row: integer);
    function GetGridClass: TAdvStringGridClass; virtual;
    procedure DoSheetChange(NewTab: integer; var AllowChange: boolean); virtual;
    procedure DoSheetChanged(NewTab: integer); virtual;
  public
    function GetVersionNr: Integer; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    property TabSet: TTabset read FTabSet;
    property Grid: TAdvStringGrid read GetGrid;
    procedure RemoveSheet(Index: Integer);
    procedure InsertSheet(Index: Integer; SheetName: string);
    procedure AddSheet(SheetName: string);
  published
    property ActiveSheet: Integer read FActiveSheet write SetActiveSheet;
    property Sheets: TGridSheetCollection read GetSheets write SetSheets;
    property TabLook: TTabLook read FTabLook write FTabLook;
    property OnSheetChange: TSheetChangeEvent read FOnSheetChange write FOnSheetChange;
    property OnSheetChanged: TSheetChangedEvent read FOnSheetChanged write FOnSheetChanged;
    property Align;
    property Anchors;
    property Constraints;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property Visible;
    property Version: string read GetVersion write SetVersion;    
  end;



implementation

type
  TGridCracker = class(TAdvStringGrid);

{ TAdvGridWorkbook }

constructor TAdvGridWorkbook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAutoCreated := false;
  FWinCreated := false;
  
  BorderWidth := 1;
  Ctl3D := false;

  FSheets := TGridSheetCollection.Create(Self);
  FSheets.Add.Name := 'Sheet 1';
  FSheets.Add.Name := 'Sheet 2';
  FSheets.Add.Name := 'Sheet 3';

  FSheets.OnChange := SheetsChanged;

  FGrid := nil;
  Width := 320;
  Height := 150;

  ControlStyle := ControlStyle + [csAcceptsControls];

  FTabLook := TTabLook.Create;
end;

procedure TAdvGridWorkbook.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := Style or WS_TABSTOP;
    Style := Style or WS_BORDER;
  end;
end;

procedure TAdvGridWorkbook.CreateWnd;
var
  i: integer;
  TabSetNeeded: boolean;
begin
  inherited;

  if FWinCreated then
    Exit;

  FWinCreated := true;

  TabSetNeeded := (FTabSet = nil);

  if ControlCount = 0 then
  begin
    FGrid := GetGridClass.Create(self);
    FGrid.Parent := self;
    FGrid.Visible := true;
    FGrid.BorderStyle := bsNone;
  end
  else
  begin
    FGrid := TAdvStringGrid(Controls[0]);
    FAutoCreated := true;
  end;

  FDefaultRowCount := FGrid.RowCount;
  FDefaultColCount := FGrid.ColCount;
  FDefaultRowHeight := FGrid.DefaultRowHeight;
  FDefaultColWidth := FGrid.DefaultColWidth;

  if TabSetNeeded then
    FTabSet := TTabSet.Create(Self);

  FTabSet.Parent := self;
  FTabSet.Align := alBottom;
  {$IFDEF DELPHI6_LVL}
  FTabSet.SoftTop := true;
  {$ENDIF}

  FGrid.Align := alClient;
  FGrid.Options := FGrid.Options + [goEditing];

//  if not FAutoCreated then
//  begin
//    FGrid.Name := self.Name + 'Grid' + inttostr(Parent.ControlCount);
//  end;

  if TabSetNeeded then
    for i := 1 to FSheets.Count do
      FTabSet.Tabs.Add(FSheets[i - 1].Name);

  if FTabSet.Tabs.Count > 0 then
    FTabSet.TabIndex := 0;

  FSheets.OnChange := SheetChange;  
  FTabSet.OnChange := TabChange;
  FTabLook.OnChange := TabLookChange;

  TabLookChange(Self);
end;

destructor TAdvGridWorkbook.Destroy;
begin
  FSheets.Free;
  FTabLook.Free;

  if (FGrid <> nil) and not FAutoCreated then
    FGrid.Free;

  FTabSet.Free;
  inherited;
end;


function TAdvGridWorkbook.GetSheets: TGridSheetCollection;
begin
  Result := FSheets;
end;

procedure TAdvGridWorkbook.NCPaintProc;
var
  DC: HDC;
  WindowBrush:hBrush;
  Canvas: TCanvas;
begin
  if BorderWidth = 0 then
    Exit;

  DC := GetWindowDC(Handle);
  WindowBrush := 0;
  try
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;

    WindowBrush := CreateSolidBrush(ColorToRGB(clRed));

    if (1>0) then
      Canvas.Pen.Color := $B99D7F
    else
      Canvas.Pen.Color := clGray;

    Canvas.MoveTo(1,Height);
    Canvas.LineTo(1,1);
    Canvas.LineTo(Width - 2,1);
    Canvas.LineTo(Width - 2,Height - 2);
    Canvas.LineTo(1,Height - 2);

    if (Parent is TWinControl) then
    begin
      Canvas.Pen.Color := (Parent as TWinControl).Brush.Color;
      Canvas.MoveTo(0,Height);
      Canvas.LineTo(0,0);
      Canvas.LineTo(Width - 1,0);
      Canvas.LineTo(Width - 1,Height - 1);
      Canvas.LineTo(0,Height-1);
    end;
    Canvas.Free;
  finally
    DeleteObject(WindowBrush);
    ReleaseDC(Handle,DC);
  end;
end;

procedure TAdvGridWorkbook.StoreActiveSheet;
var
  i: Integer;
begin
  FGrid.UnHideColumnsAll;
  FSheets[FActiveSheet].Data.Clear;
  FGrid.SaveFixedCells := True;
  FGrid.ExpandAll;   // required to make sure all rows are saved

  TGridCracker(FGrid).InWorkbook := true;

  FGrid.SaveToBinStream(FSheets[FActiveSheet].Data);

  TGridCracker(FGrid).InWorkbook := false;

  FSheets[FActiveSheet].Row := FGrid.Row;
  FSheets[FActiveSheet].Col := FGrid.Col;
  FSheets[FActiveSheet].TopRow := FGrid.TopRow;
  FSheets[FActiveSheet].LeftCol := FGrid.LeftCol;
  FSheets[FActiveSheet].Sortcolumn := FGrid.SortSettings.Column;

  FSheets[FActiveSheet].RowHeights.Clear;
  for i := 1 to FGrid.RowCount do
    FSheets[FActiveSheet].RowHeights.Add(FGrid.RowHeights[i - 1]);

  FSheets[FActiveSheet].ColWidths.Clear;
  for i := 1 to FGrid.ColCount do
    FSheets[FActiveSheet].ColWidths.Add(FGrid.ColWidths[i - 1]);

end;

procedure TAdvGridWorkbook.SetActiveSheet(const Value: Integer);
var
  i: Integer;
  sfc: Boolean;
begin
  if not Assigned(Grid) then
    Exit;

  if (FActiveSheet <> Value) and (Value >= 0) and (Value < Sheets.Count) then
  begin
    FGrid.HideInplaceEdit;
    sfc := FGrid.SaveFixedCells;
    if FActiveSheet >= 0 then
    begin
      StoreActiveSheet;
      {
      FSheets[FActiveSheet].Data.Clear;
      FGrid.SaveFixedCells := True;
      FGrid.SaveToBinStream(FSheets[FActiveSheet].Data);
      FSheets[FActiveSheet].Row := FGrid.Row;
      FSheets[FActiveSheet].Col := FGrid.Col;
      FSheets[FActiveSheet].TopRow := FGrid.TopRow;
      FSheets[FActiveSheet].LeftCol := FGrid.LeftCol;
      FSheets[FActiveSheet].Sortcolumn := FGrid.SortSettings.Column;

      FSheets[FActiveSheet].RowHeights.Clear;
      for i := 1 to FGrid.RowCount do
        FSheets[FActiveSheet].RowHeights.Add(FGrid.RowHeights[i - 1]);

      FSheets[FActiveSheet].ColWidths.Clear;
      for i := 1 to FGrid.ColCount do
        FSheets[FActiveSheet].ColWidths.Add(FGrid.ColWidths[i - 1]);
      }
    end;

    if FSheets[Value].Data.Size <> 0 then
    begin
      FSheets[Value].Data.Position := 0;
      FGrid.LoadFromBinStream(FSheets[Value].Data);

      RestoreSelection(FSheets[Value].Col, FSheets[Value].Row);

      FGrid.TopRow := FSheets[Value].TopRow;
      FGrid.LeftCol := FSheets[Value].LeftCol;

      for i := 1 to FSheets[Value].ColWidths.Count do
        if i < FGrid.ColCount then
          FGrid.ColWidths[i - 1] := FSheets[Value].ColWidths[i - 1];

      for i := 1 to FSheets[Value].RowHeights.Count do
        if i < FGrid.RowCount then
          FGrid.RowHeights[i - 1] := FSheets[Value].RowHeights[i - 1];

      FGrid.SortSettings.Column := FSheets[Value].SortColumn;
    end
    else
    begin
      FGrid.Clear;
      FGrid.ColCount := FDefaultColCount;
      FGrid.RowCount := FDefaultRowCount;

      RestoreSelection(FSheets[Value].Col, FSheets[Value].Row);

      FGrid.TopRow := FGrid.FixedRows;
      FGrid.LeftCol := FGrid.FixedCols;
      FGrid.DefaultRowHeight := FDefaultRowHeight;
      FGrid.DefaultColWidth := FDefaultColWidth;
    end;

    // restore saved value
    FGrid.SaveFixedCells := sfc;

    FActiveSheet := Value;

    FProgSheetChange := true;
    try
      FTabSet.TabIndex := Value;
    finally
      FProgSheetChange := false;
    end;
  end;
end;


procedure TAdvGridWorkbook.SetName(const Value: TComponentName);
begin
  inherited;
  if Assigned(FGrid) then
    FGrid.Name := Value + 'Grid';
end;

procedure TAdvGridWorkbook.SetSheets(const Value: TGridSheetCollection);
begin
  FSheets.Assign(Value);
end;

procedure TAdvGridWorkbook.SheetChange(Sender: TObject);
var
  i: integer;
begin
  while FTabSet.Tabs.Count > FSheets.Count do
    FTabSet.Tabs.Delete(FTabSet.Tabs.Count - 1);

  while FTabSet.Tabs.Count < FSheets.Count do
    FTabSet.Tabs.Add('');

  for i := 1 to FSheets.Count do
  begin
    FTabSet.Tabs[i - 1] := FSheets[i - 1].Name;
  end;
end;


procedure TAdvGridWorkbook.DoSheetChange(NewTab: integer; var AllowChange: boolean);
begin
  if Assigned(FOnSheetChange) then
    FOnSheetChange(Self, NewTab, AllowChange);
end;

procedure TAdvGridWorkbook.DoSheetChanged(NewTab: integer);
begin
  if Assigned(FOnSheetChanged) then
    FOnSheetChanged(Self, NewTab);
end;

procedure TAdvGridWorkbook.TabChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  if not FProgSheetChange then
    DoSheetChange(NewTab, AllowChange);

  if AllowChange then
    ActiveSheet := NewTab;

  if not FProgSheetChange then
    DoSheetChanged(NewTab);
end;

procedure TAdvGridWorkbook.WMNCPaint(var Message: TMessage);
begin
  inherited;
  NCPaintProc;
  Message.Result := 0;
end;

procedure TAdvGridWorkbook.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
begin
  inherited;
  if not FAutoCreated and FWinCreated then
    Proc(FGrid);
end;

function TAdvGridWorkbook.GetGrid: TAdvStringGrid;
begin
  Result := FGrid;

  if (csLoading in ComponentState) then
    Exit;

  if not HandleAllocated then
    CreateWnd;

  Result := FGrid;
end;

function TAdvGridWorkbook.GetGridClass: TAdvStringGridClass;
begin
  Result := TAdvStringGrid;
end;

procedure TAdvGridWorkbook.SheetsChanged(Sender: TObject);
begin
  if (ActiveSheet = -1) and (Sheets.Count > 0) then
    ActiveSheet := Sheets.Count - 1;

  if Sheets.Count = 0 then
    ActiveSheet := -1;
end;

procedure TAdvGridWorkbook.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FGrid) then
    FGrid := nil;
end;

procedure TAdvGridWorkbook.TabLookChange(Sender: TObject);
begin
  FTabSet.SelectedColor := FTabLook.SelectColor;
  FTabSet.UnSelectedColor := FTabLook.UnSelectColor;
  FTabSet.DitherBackground := FTabLook.DitherBackground;
  FTabSet.BackgroundColor := FTabLook.BackgroundColor;
  FTabSet.Font.Assign(FTabLook.Font);
end;

procedure TAdvGridWorkbook.AddSheet(SheetName: string);
begin
  FSheets.Add.Name := SheetName;
  ActiveSheet := FSheets.Count - 1;
end;

procedure TAdvGridWorkbook.InsertSheet(Index: Integer; SheetName: string);
begin
  if Index < Sheets.Count then
  begin
    StoreActiveSheet;
    FActiveSheet := -1;
    FSheets.Insert(Index).Name := SheetName;
    ActiveSheet := Index;
  end;
end;

procedure TAdvGridWorkbook.Loaded;
begin
  inherited;
end;

procedure TAdvGridWorkbook.RemoveSheet(Index: Integer);
begin
  if Index < Sheets.Count then
  begin
    ActiveSheet := Index;
    if FTabSet<>nil then
      FTabSet.Tabs.Delete(Index);
    FSheets.Items[Index].Free;
    FActiveSheet := -1;
    ActiveSheet := 0;
  end;
end;


procedure TAdvGridWorkbook.RestoreSelection(Col, Row: integer);
begin
  if (Col < FGrid.ColCount) and (Row < FGrid.RowCount) and (Col >= FGrid.FixedCols) and (Row >= FGrid.FixedRows) then
    FGrid.Selection := TGridRect(Rect(Col,Row,Col,Row));
end;

function TAdvGridWorkbook.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

procedure TAdvGridWorkbook.SetVersion(const Value: string);
begin

end;

function TAdvGridWorkbook.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

{ TGridSheetCollection }

function TGridSheetCollection.Add: TGridSheet;
begin
  Result := TGridSheet(inherited Add);
end;

procedure TGridSheetCollection.Clear;
begin
  while (Count > 0) do
    FOwner.RemoveSheet(0);
end;

constructor TGridSheetCollection.Create(AOwner: TAdvGridWorkbook);
begin
  inherited Create(TGridSheet);
  FOwner := AOwner;
end;

function TGridSheetCollection.GetItem(Index: Integer): TGridSheet;
begin
  Result := TGridSheet(inherited Items[Index]);
end;

function TGridSheetCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TGridSheetCollection.Insert(Index: Integer): TGridSheet;
begin
  Result := TGridSheet(inherited Insert(Index));
end;

procedure TGridSheetCollection.SetItem(Index: Integer;
  const Value: TGridSheet);
begin
  inherited Items[Index] := Value;
end;

procedure TGridSheetCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(OnChange) then
    OnChange(Self); 
end;

{ TGridSheet }

procedure TGridSheet.Changed;
begin
  if Assigned(TGridSheetCollection(Collection).OnChange) then
    TGridSheetCollection(Collection).OnChange(Collection);
end;

constructor TGridSheet.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FData := TMemoryStream.Create;
  FColWidths := TIntList.Create(0,0);
  FRowHeights := TIntList.Create(0,0);
  FSortColumn := -1;
  Changed;
end;

destructor TGridSheet.Destroy;
begin
  Changed;
  FData.Free;
  FColWidths.Free;
  FRowHeights.Free;
  inherited;
end;

procedure TGridSheet.SetName(const Value: string);
begin
  FName := Value;
  Changed;
end;

{ TTabLook }

constructor TTabLook.Create;
begin
  inherited;
  FFont := TFont.Create;
  FSelectColor := clBtnFace;
  FUnSelectColor := clWhite;
  FDitherBackground := true;
  FBackgroundColor := clBtnFace;
  FFont.OnChange := FontChanged;
end;

destructor TTabLook.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TTabLook.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TTabLook.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
  Changed;
end;

procedure TTabLook.SetDitherBackground(const Value: Boolean);
begin
  FDitherBackground := Value;
  Changed;
end;

procedure TTabLook.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TTabLook.SetSelectColor(const Value: TColor);
begin
  FSelectColor := Value;
  Changed;
end;

procedure TTabLook.SetUnSelectColor(const Value: TColor);
begin
  FUnSelectColor := Value;
  Changed;
end;

procedure TTabLook.FontChanged(Sender: TObject);
begin
  Changed;
end;

end.
