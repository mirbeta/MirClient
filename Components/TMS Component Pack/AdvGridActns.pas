{***************************************************************************}
{ TAdvStringGrid actions                                                    }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2015                                               }
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

unit AdvGridActns;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvGrid, ActnList, Graphics, RTTI, Windows, SysUtils
  {$IFDEF DELPHIXE3_LVL}
  , System.Actions
  {$ENDIF}
  ;

type
  TAdvStringGridAction = class(TAction)
  private
    FControl: TAdvStringGrid;
    procedure SetControl(Value: TAdvStringGrid);
  protected
    function GetControl(Target: TObject): TAdvStringGrid; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    property Control: TAdvStringGrid read FControl write SetControl;
  end;

  TAdvStringGridCut = class(TAdvStringGridAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TAdvStringGridCopy = class(TAdvStringGridAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TAdvStringGridPaste = class(TAdvStringGridAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TAdvStringGridBold = class(TAdvStringGridAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvStringGridItalic = class(TAdvStringGridAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvStringGridUnderline = class(TAdvStringGridAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvStringGridStrikeOut = class(TAdvStringGridAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvStringGridAlignLeft = class(TAdvStringGridAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvStringGridAlignCenter = class(TAdvStringGridAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvStringGridAlignRight = class(TAdvStringGridAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvStringGridColorAction = class(TAdvStringGridAction)
  protected
    function GetActionComponentColor: TColor;
    procedure SetActionComponentColor(AColor: TColor);
  end;

  TAdvStringGridTextColor = class(TAdvStringGridColorAction)
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvStringGridColor = class(TAdvStringGridColorAction)
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvStringGridFontName = class(TAdvStringGridAction)
  protected
    function GetActionComponentFontName: string;
    procedure SetActionComponentFontName(AFontName: string);
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvStringGridFontSize = class(TAdvStringGridAction)
  protected
    function GetActionComponentFontSize: integer;
    procedure SetActionComponentFontSize(AFontSize: integer);
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

procedure Register;

implementation

{ TAdvStringGridAction }

destructor TAdvStringGridAction.Destroy;
begin
  if Assigned(FControl) then
    FControl.RemoveFreeNotification(Self);

  inherited;
end;

function TAdvStringGridAction.GetControl(Target: TObject): TAdvStringGrid;
begin
  Result := Target as TAdvStringGrid;
end;

function TAdvStringGridAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((Control <> nil) and (Target = Control) or
    (Control = nil) and (Target is TAdvStringGrid)) and TAdvStringGrid(Target).Focused;
end;

procedure TAdvStringGridAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Control) then
    Control := nil;
end;

procedure TAdvStringGridAction.SetControl(Value: TAdvStringGrid);
begin
  if Value <> FControl then
  begin
    FControl := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

procedure TAdvStringGridAction.UpdateTarget(Target: TObject);
begin
  inherited;
end;

{ TAdvStringGridBold }

procedure TAdvStringGridBold.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
  begin
    SetFontStyle(Selection, fsBold, not IsFontStyle(Selection, fsBold));
  end;

end;

procedure TAdvStringGridBold.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvStringGrid) and TAdvStringGrid(Target).Focused;
  with GetControl(Target) do
    Checked := IsFontStyle(Selection, fsBold);
end;

{ TAdvStringGridItalic }

procedure TAdvStringGridItalic.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
  begin
    SetFontStyle(Selection, fsItalic, not IsFontStyle(Selection, fsItalic));
  end;
end;

procedure TAdvStringGridItalic.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvStringGrid) and TAdvStringGrid(Target).Focused;
  with GetControl(Target) do
    Checked := IsFontStyle(Selection, fsItalic);
end;

{ TAdvStringGridUnderline }

procedure TAdvStringGridUnderline.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
  begin
    SetFontStyle(Selection, fsUnderline, not IsFontStyle(Selection, fsUnderline));
  end;
end;

procedure TAdvStringGridUnderline.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvStringGrid) and TAdvStringGrid(Target).Focused;
  with GetControl(Target) do
    Checked := IsFontStyle(Selection, fsUnderline);
end;

{ TAdvStringGridStrikeOut }

procedure TAdvStringGridStrikeOut.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
  begin
    SetFontStyle(Selection, fsStrikeOut, not IsFontStyle(Selection, fsStrikeOut));
  end;
end;

procedure TAdvStringGridStrikeOut.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvStringGrid) and TAdvStringGrid(Target).Focused;
  with GetControl(Target) do
    Checked := IsFontStyle(Selection, fsStrikeOut);
end;

{ TAdvStringGridCut }

procedure TAdvStringGridCut.ExecuteTarget(Target: TObject);
begin
  inherited;
  GetControl(Target).CutSelectionToClipboard;
end;

procedure TAdvStringGridCut.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvStringGrid) and TAdvStringGrid(Target).Focused;
end;

{ TAdvStringGridCopy }

procedure TAdvStringGridCopy.ExecuteTarget(Target: TObject);
begin
  inherited;
  GetControl(Target).CopySelectionToClipboard;
end;

procedure TAdvStringGridCopy.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvStringGrid) and TAdvStringGrid(Target).Focused;
end;

{ TAdvStringGridPaste }

procedure TAdvStringGridPaste.ExecuteTarget(Target: TObject);
begin
  inherited;
  GetControl(Target).PasteSelectionFromClipboard;
end;

procedure TAdvStringGridPaste.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvStringGrid) and TAdvStringGrid(Target).Focused;
end;

{ TAdvStringGridAlignLeft }

procedure TAdvStringGridAlignLeft.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
    SetAlignment(Selection, taLeftJustify);
end;

procedure TAdvStringGridAlignLeft.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvStringGrid) and TAdvStringGrid(Target).Focused;
end;

{ TAdvStringGridAlignCenter }

procedure TAdvStringGridAlignCenter.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
    SetAlignment(Selection, taCenter);
end;

procedure TAdvStringGridAlignCenter.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvStringGrid) and TAdvStringGrid(Target).Focused;
end;

{ TAdvStringGridAlignRight }

procedure TAdvStringGridAlignRight.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
    SetAlignment(Selection, taRightJustify);
end;

procedure TAdvStringGridAlignRight.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvStringGrid) and TAdvStringGrid(Target).Focused;
end;


{ TAdvStringGridColorAction }

function TAdvStringGridColorAction.GetActionComponentColor: TColor;
var
  AContext: TRttiContext;
  rt: TRttiType;
  prop: TRttiProperty;
  v: TValue;
begin
  Result := clNone;

  AContext := TRttiContext.Create;

  try
    rt := AContext.GetType(ActionComponent.ClassType);

    prop := rt.GetProperty('Color');

    if Assigned(prop) then
    begin
      v := prop.GetValue(ActionComponent);
      Result := TColor(v.AsInteger);
    end
    else
    begin
      prop := rt.GetProperty('SelectedColor');
      if Assigned(prop) then
      begin
        v := prop.GetValue(ActionComponent);
        Result := TColor(v.AsInteger);
      end;
    end;
  finally
    AContext.Free;
  end;
end;


procedure TAdvStringGridColorAction.SetActionComponentColor(AColor: TColor);
var
  AContext: TRttiContext;
  rt: TRttiType;
  prop: TRttiProperty;
  v: TValue;
begin
  AContext := TRttiContext.Create;

  try
    rt := AContext.GetType(ActionComponent.ClassType);

    prop := rt.GetProperty('Color');

    if Assigned(prop) then
    begin
      v := AColor;
      prop.SetValue(ActionComponent, v);
    end
    else
    begin
      prop := rt.GetProperty('SelectedColor');
      if Assigned(prop) then
      begin
        v := AColor;
        prop.SetValue(ActionComponent, v);
      end;
    end;
  finally
    AContext.Free;
  end;
end;

{ TAdvStringGridTextColor }

procedure TAdvStringGridTextColor.ExecuteTarget(Target: TObject);
var
  clr: TColor;
begin
  inherited;
  with GetControl(Target) do
  begin
    clr := GetActionComponentColor;

    if clr <> clNone then
      SetCellTextColor(Selection, clr);
  end;
end;

function TAdvStringGridTextColor.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((Control <> nil) and (Target = Control) or
    (Control = nil) and (Target is TAdvStringGrid));
end;

procedure TAdvStringGridTextColor.UpdateTarget(Target: TObject);
var
  clr: TColor;
begin
  inherited;
  Enabled := (Target is TAdvStringGrid); // and TAdvStringGrid(Target).Focused;

  if Enabled and TAdvStringGrid(Target).Focused then
  begin
    with (Target as TAdvStringGrid) do
    begin
      clr := FontColors[Col,Row];
      if clr = clNone then
        clr := (Target as TAdvStringGrid).Font.Color;

      SetActionComponentColor(clr);
    end;
  end;

end;

{ TAdvStringGridColor }

procedure TAdvStringGridColor.ExecuteTarget(Target: TObject);
var
  clr: TColor;
begin
  inherited;

  with GetControl(Target) do
  begin
    clr := GetActionComponentColor;

    if clr <> clNone then
      SetCellColor(Selection, clr);
  end;
end;

function TAdvStringGridColor.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((Control <> nil) and (Target = Control) or
    (Control = nil) and (Target is TAdvStringGrid));
end;

procedure TAdvStringGridColor.UpdateTarget(Target: TObject);
var
  clr: TColor;
begin
  inherited;
  Enabled := (Target is TAdvStringGrid); // and TAdvStringGrid(Target).Focused;

  if Enabled and TAdvStringGrid(Target).Focused then
  begin
    with (Target as TAdvStringGrid) do
    begin
      clr := Colors[Col,Row];
      if clr = clNone then
        clr := (Target as TAdvStringGrid).Color;

      SetActionComponentColor(clr);
    end;
  end;
end;


{ TAdvStringGridFontName }

procedure TAdvStringGridFontName.ExecuteTarget(Target: TObject);
var
  fn: string;
begin
  inherited;

  with GetControl(Target) do
  begin
    fn := GetActionComponentFontName;
    if fn <> '' then
      SetCellFontName(Selection, fn);
  end;
end;

function TAdvStringGridFontName.GetActionComponentFontName: string;
var
  AContext: TRttiContext;
  rt: TRttiType;
  prop: TRttiProperty;
  v: TValue;
begin
  Result := '';

  AContext := TRttiContext.Create;

  try
    rt := AContext.GetType(ActionComponent.ClassType);

    prop := rt.GetProperty('SelectedFontName');

    if Assigned(prop) then
    begin
      v := prop.GetValue(ActionComponent);
      Result := v.AsString;
    end;
  finally
    AContext.Free;
  end;

end;

function TAdvStringGridFontName.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((Control <> nil) and (Target = Control) or
    (Control = nil) and (Target is TAdvStringGrid));
end;

procedure TAdvStringGridFontName.SetActionComponentFontName(AFontName: string);
var
  AContext: TRttiContext;
  rt: TRttiType;
  prop: TRttiProperty;
  v: TValue;
begin
  AContext := TRttiContext.Create;

  try
    rt := AContext.GetType(ActionComponent.ClassType);

    prop := rt.GetProperty('SelectedFontName');

    if Assigned(prop) then
    begin
      v := AFontName;
      prop.SetValue(ActionComponent, v);
    end;
  finally
    AContext.Free;
  end;

end;

procedure TAdvStringGridFontName.UpdateTarget(Target: TObject);
var
  fn: string;
begin
  inherited;
  Enabled := (Target is TAdvStringGrid); // and TAdvStringGrid(Target).Focused;

  if Enabled and TAdvStringGrid(Target).Focused then
  begin
    with (Target as TAdvStringGrid) do
    begin
      fn := FontNames[Col,Row];
      SetActionComponentFontName(fn);
    end;
  end;
end;

{ TAdvStringGridFontSize }

procedure TAdvStringGridFontSize.ExecuteTarget(Target: TObject);
var
  fs: integer;
begin
  inherited;

  with GetControl(Target) do
  begin
    fs := GetActionComponentFontSize;
    if fs > 0 then
      SetCellFontSize(Selection, fs);
  end;
end;

function TAdvStringGridFontSize.GetActionComponentFontSize: integer;
var
  AContext: TRttiContext;
  rt: TRttiType;
  prop: TRttiProperty;
  v: TValue;
begin
  Result := -1;

  AContext := TRttiContext.Create;

  try
    rt := AContext.GetType(ActionComponent.ClassType);

    prop := rt.GetProperty('SelectedFontSize');

    if Assigned(prop) then
    begin
      v := prop.GetValue(ActionComponent);
      Result := v.AsInteger;
    end;
  finally
    AContext.Free;
  end;

end;

function TAdvStringGridFontSize.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((Control <> nil) and (Target = Control) or
    (Control = nil) and (Target is TAdvStringGrid));
end;

procedure TAdvStringGridFontSize.SetActionComponentFontSize(AFontSize: integer);
var
  AContext: TRttiContext;
  rt: TRttiType;
  prop: TRttiProperty;
  v: TValue;
begin
  AContext := TRttiContext.Create;

  try
    rt := AContext.GetType(ActionComponent.ClassType);

    prop := rt.GetProperty('SelectedFontSize');

    if Assigned(prop) then
    begin
      v := AFontSize;
      prop.SetValue(ActionComponent, v);
    end;
  finally
    AContext.Free;
  end;
end;

procedure TAdvStringGridFontSize.UpdateTarget(Target: TObject);
var
  fs: integer;
begin
  inherited;
  Enabled := (Target is TAdvStringGrid); // and TAdvStringGrid(Target).Focused;

  if Enabled and TAdvStringGrid(Target).Focused then
  begin
    with (Target as TAdvStringGrid) do
    begin
      fs := FontSizes[Col,Row];
      SetActionComponentFontSize(fs);
    end;
  end;
end;



procedure Register;
begin
  RegisterActions('AdvStringGrid',[
    TAdvStringGridItalic,
    TAdvStringGridBold,
    TAdvStringGridUnderline,
    TAdvStringGridStrikeOut,
    TAdvStringGridCut,
    TAdvStringGridCopy,
    TAdvStringGridPaste,
    TAdvStringGridAlignLeft,
    TAdvStringGridAlignCenter,
    TAdvStringGridAlignRight,
    TAdvStringGridColor,
    TAdvStringGridTextColor,
    TAdvStringGridFontName,
    TAdvStringGridFontSize
    ], nil);
end;



end.
