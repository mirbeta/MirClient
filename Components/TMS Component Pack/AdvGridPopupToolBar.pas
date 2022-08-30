{*************************************************************************}
{ TMS TAdvGridToolBar Popup                                               }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2015                                              }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvGridPopupToolBar;

interface

uses
  Classes, AdvGrid, AdvToolBarPopup, AdvToolBar, Graphics, Controls, Types,
  Grids, RTTI;

type

  TAdvGridPopupToolBarWindow = class(TAdvPopupToolBarWindow)
  private
    FGrid: TAdvStringGrid;
    FGridCell: TGridCoord;
  protected
    function DoFontStyle(AType: TFontStyleType): boolean; override;
    procedure DoFontSize(ASize: integer); override;
    procedure DoFont(AName: string); override;
    procedure DoFontColor(AColor: TColor); override;
    procedure DoColor(AColor: TColor); override;
    function DoAlign(AAlign: TAlignment): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Grid: TAdvStringGrid read FGrid write FGrid;
    property GridCell: TGridCoord read FGridCell write FGridCell;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGridPopupToolBar = class(TAdvGridPopup)
  private
    FPopupWindow: TAdvGridPopupToolBarWindow;
    FToolBarStyler: TCustomAdvToolBarStyler;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show(PT: TPoint); override;
    procedure Hide; override;
    function MouseInPopup(PT: TPoint): boolean; override;
  published
    property ToolBarStyler: TCustomAdvToolBarStyler read FToolBarStyler write FToolBarStyler;
  end;

procedure Register;

implementation

type
  TWinControlEx = class(TWinControl);


procedure Register;
begin
  RegisterComponents('TMS Grids',[TAdvGridPopupToolBar]);
end;

{ TAdvGridPopupToolBarWindow }

constructor TAdvGridPopupToolBarWindow.Create(AOwner: TComponent);
begin
  Options := [oBold,oItalic,oUnderline,oStrikeThrough,oColor,oFontColor,oAlignLeft,oAlignCenter,oAlignRight];
  inherited;
end;

function TAdvGridPopupToolBarWindow.DoAlign(AAlign: TAlignment): boolean;
begin
  Grid.Alignments[GridCell.X, GridCell.Y] := AAlign;
  Result := false;
end;

procedure TAdvGridPopupToolBarWindow.DoColor(AColor: TColor);
var
  wc: TWinControl;
begin
  Grid.Colors[GridCell.X, GridCell.Y] := AColor;
  if Grid.EditMode then
  begin
    wc := Grid.CellEditor;
    TWinControlEx(wc).Color := AColor;
  end;
end;

procedure TAdvGridPopupToolBarWindow.DoFont(AName: string);
var
  wc: TWinControl;
begin
  Grid.FontNames[GridCell.X, GridCell.Y] := AName;
  if Grid.EditMode then
  begin
    wc := Grid.CellEditor;
    TWinControlEx(wc).Font.Name := AName;
  end;
end;

procedure TAdvGridPopupToolBarWindow.DoFontColor(AColor: TColor);
var
  wc: TWinControl;
begin
  Grid.FontColors[GridCell.X, GridCell.Y] := AColor;
  if Grid.EditMode then
  begin
    wc := Grid.CellEditor;
    TWinControlEx(wc).Font.Color := AColor;
  end;
end;

procedure TAdvGridPopupToolBarWindow.DoFontSize(ASize: integer);
var
  wc: TWinControl;
begin
  Grid.FontSizes[GridCell.X, GridCell.Y] := ASize;
  if Grid.EditMode then
  begin
    wc := Grid.CellEditor;
    TWinControlEx(wc).Font.Size := ASize;
  end;
end;

function TAdvGridPopupToolBarWindow.DoFontStyle(AType: TFontStyleType): boolean;

  function ToggleStyle(fntstyle: TFontStyle): boolean;
  var
    fs: TFontStyles;
    wc: TWinControl;
  begin
    fs := Grid.FontStyles[GridCell.X, GridCell.Y];

    if fntstyle in fs then
      fs := fs - [fntstyle]
    else
      fs := fs + [fntstyle];

    Grid.FontStyles[GridCell.X, GridCell.Y] := fs;
    Result := fntstyle in fs;

    if Grid.EditMode then
    begin
      wc := Grid.CellEditor;
      TWinControlEx(wc).Font.Style := fs;
    end;
  end;

begin
  case AType of
  fstBold: Result := ToggleStyle(fsBold);
  fstItalic: Result := ToggleStyle(fsItalic);
  fstUnderline: Result := ToggleStyle(fsUnderline);
  fstStrikeThrough: Result := ToggleStyle(fsStrikeOut)
  else
    Result := false;
  end;
end;

{ TAdvGridPopupToolBar }

constructor TAdvGridPopupToolBar.Create(AOwner: TComponent);
begin
  inherited;

  FPopupWindow := TAdvGridPopupToolBarWindow.Create(Self);
end;

destructor TAdvGridPopupToolBar.Destroy;
begin

  inherited;
end;

procedure TAdvGridPopupToolBar.Hide;
begin
  inherited;
  FPopupWindow.Visible := false;
  FPopupWindow.Parent := nil;
end;

function TAdvGridPopupToolBar.MouseInPopup(PT: TPoint): boolean;
begin
  Result := PtInRect(Rect(FPopupWindow.Left, FPopupWindow.Top, FPopupWindow.Left + FPopupWindow.Width, FPopupWindow.Top + FPopupWindow.Height), PT);
end;

procedure TAdvGridPopupToolBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FToolBarStyler) then
    FToolBarStyler := nil;
end;

procedure TAdvGridPopupToolBar.Show(PT: TPoint);
var
  fs: TFontStyles;
begin
  inherited;
  FPopupWindow.Left := PT.X;
  FPopupWindow.Top := PT.Y;
  FPopupWindow.Parent := Grid;
  FPopupWindow.Grid := Grid;
  FPopupWindow.GridCell := GridCell;
  FPopupWindow.ToolBarStyler := ToolBarStyler;

  // initialize to current grid cell state
  fs := Grid.FontStyles[GridCell.X, GridCell.Y];
  FPopupWindow.SetFontStyleState(fstBold, fsBold in fs);
  FPopupWindow.SetFontStyleState(fstItalic, fsItalic in fs);
  FPopupWindow.SetFontStyleState(fstUnderline, fsUnderline in fs);
  FPopupWindow.SetFontStyleState(fstStrikeThrough, fsStrikeOut in fs);

  FPopupWindow.SetFontColor(grid.FontColors[GridCell.X, GridCell.Y]);
  FPopupWindow.SetColor(grid.Colors[GridCell.X, GridCell.Y]);
  FPopupWindow.SetFontSize(grid.FontSizes[GridCell.X, GridCell.Y]);
  FPopupWindow.SetFont(grid.FontNames[GridCell.X, GridCell.Y]);

  FPopupWindow.Visible := true;
  FPopupWindow.Width :=  FPopupWindow.Width + 1;
  FPopupWindow.Width :=  FPopupWindow.Width - 1;

end;

end.
