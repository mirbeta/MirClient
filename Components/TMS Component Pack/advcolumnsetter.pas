{***************************************************************************}
{ TAdvStringGrid component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 1996-2010                                          }
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

unit AdvColumnSetter;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, BaseGrid, AdvGrid, Grids, ExtCtrls, AdvObj
  {$IFDEF DELPHI6_LVL}
  , Types
  {$ENDIF}
  ;

type
  TMouseState = (msNone, msDown, msMoving);

  TColumnSetterInsertEvent = procedure(Sender: TObject; Position: integer) of object;
  TColumnSettingChangedEvent = procedure(Sender: TObject; Index: integer; Position: integer) of object;
  TColumnSettingDeletedEvent = procedure(Sender: TObject; Index: integer) of object;

type
  TAdvColumnSetter = class(TCustomControl)
  private
    FColumns: TIntList;
    FState: TMouseState;
    FMouseX: Integer;
    FMouseIdx: Integer;
    FGrid: TAdvStringGrid;
    FOffset: Integer;
    FMaxLength: Integer;
    FColumnSetterInsert: TColumnSetterInsertEvent;
    FColumnSetterChanged: TColumnSettingChangedEvent;
    FColumnSetterChanging: TColumnSettingChangedEvent;
    FColumnSetterDeleted: TColumnSettingDeletedEvent;
    procedure SetGrid(const Value: TAdvStringGrid);
    procedure SetMaxLength(const Value: Integer);
    procedure SetOffset(const Value: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure PaintColumnLine(X: Integer; Index: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWnd; override;
  public
    ColWidth: Integer;
    StepWidth: Integer;
    constructor Create(AOwner : TComponent); override;
  published
    property Offset: Integer read FOffset write SetOffset;   
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    property Columns: TIntList read FColumns;
    property Grid: TAdvStringGrid read FGrid write SetGrid;
    property OnColumnSetterInsert: TColumnSetterInsertEvent read FColumnSetterInsert write FColumnSetterInsert;
    property OnColumnSetterChanged: TColumnSettingChangedEvent read FColumnSetterChanged write FColumnSetterChanged;
    property OnColumnSetterChanging: TColumnSettingChangedEvent read FColumnSetterChanging write FColumnSetterChanging;
    property OnColumnSetterDeleted: TColumnSettingDeletedEvent read FColumnSetterDeleted write FColumnSetterDeleted;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
  end;

procedure Register;

implementation

procedure TAdvColumnSetter.PaintColumnLine(X: Integer; Index: Integer);
var
  pts: array[0..2] of TPoint;
begin
  Canvas.Brush.Color := clWhite;
  //SET POLYGON COLOR
  if (FState = msMoving) then
  begin
    if (Index = FMouseIdx) and (FMouseIdx <> -1) then
      Canvas.Brush.Color := clBlack;
  end;
  //FILL POINTS
  pts[0] := Point(X + 5, (Self.Height div 2) - 10);
  pts[2] := Point(X - 5, (Self.Height div 2) - 10);
  pts[1] := Point(X,Self.Height div 2);
  //DRAW POLYGON
  Canvas.Polygon(pts);
  //DRAW LINE
  Canvas.MoveTo(X,(Self.Height div 2));
  Canvas.LineTo(X, 400);

end;
procedure TAdvColumnSetter.SetGrid(const Value: TAdvStringGrid);
begin
  if (FGrid <> Value) then
  begin
    FGrid := Value;
    Invalidate;
  end;
end;

procedure TAdvColumnSetter.SetMaxLength(const Value: Integer);
begin
  if (FMaxLength <> Value) then
  begin
    FMaxLength := Value;
    Invalidate;
  end;
end;

procedure TAdvColumnSetter.SetOffset(const Value: Integer);
begin
  if (FOffset <> Value) then
  begin
    FOffset := Value;
    Invalidate;
  end;
end;

procedure TAdvColumnSetter.CreateWnd;
begin
  inherited;
  Canvas.Font.Name := 'Courier New';
  Canvas.Font.Size := 9;
  ColWidth := Canvas.TextWidth('W');
  StepWidth := ColWidth * 10;
end;

procedure TAdvColumnSetter.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Grid <> nil) and (MaxLength <> 0)  then
  begin
    FState := msDown;
    FMouseX := X;
    if Cursor = crDefault then
    begin
      // check if position already added !!!

      //PAINT LINE
      //PaintColumnLine(X);
      //ADD TO INTLIST
      Canvas.Brush.Color := clWhite;
      Columns.Add(X div ColWidth);
      //SET STATE TO MSDOWN
      FState := msDown;
      Invalidate;

      if Assigned(OnColumnSetterInsert) then
        OnColumnSetterInsert(Self, x div ColWidth);
    end;
  end;
end;

procedure TAdvColumnSetter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited;


  Cursor := crDefault;
  if (Grid <> nil) and (MaxLength <> 0) then
  begin
    if FState <> msMoving then
    begin
      for I := 0 to Columns.Count - 1 do
      begin
        if (Columns[I] * ColWidth < (X + 5)) and (Columns[I] * ColWidth > (X - 5)) then
        begin
          FMouseIdx := I;
          Cursor := crHandPoint;
        end;
      end;
    end;

    if (FMouseIdx <> -1) then
    begin
      if (Y > Self.Height) then
      begin
      //Delete Item

        Columns.Delete(FMouseIdx);
        FMouseIdx := -1;

        if Assigned(OnColumnSetterDeleted) then
          OnColumnSetterDeleted(Self, FMouseIdx);

        Self.Invalidate;
      end
      else
      begin
        if (FState = msDown) and (Cursor = crHandPoint) then
        begin
          FState := msMoving;
        end;

        if FState = msMoving then
        begin
          if Abs(FMouseX - X) > 5 then
          begin
            Columns[FMouseIdx] := X div ColWidth;
            Self.Invalidate;
            if Assigned(OnColumnSetterChanging) then
              OnColumnSetterChanging(Self, FMouseIdx, Columns[FMouseIdx]);
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvColumnSetter.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if (FMouseIdx <> -1) and (FState = msMoving) then
  begin
    if Assigned(OnColumnSetterChanged) then
        OnColumnSetterChanged(Self, FMouseIdx, Columns[FMouseIdx]);
  end;
  FState := msNone;

end;

procedure TAdvColumnSetter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
    if (AComponent = FGrid) then
      FGrid := nil;
  end;

  inherited;
end;

procedure TAdvColumnSetter.Paint;
var
  I, J, IMax: Integer;
  OldBkMode : integer;
begin
  if (Grid <> nil) and (MaxLength <> 0) then
  begin
    //Draw OffsetNumbers
    //CALCULATE STEPS
    IMax := FMaxLength div 10;
    //STEP
    if Self.Enabled = false then
      Canvas.Font.Color := clGray
    else
      Canvas.Font.Color := clBlack;

    Canvas.Brush.Style := bsSolid;
    Canvas.Font.Size := 10;
    OldBkMode := SetBkMode(Canvas.Handle, TRANSPARENT);
    for I := 0 to IMax do // I := 0 = Offset IMax = Maximum (NOT DRAWN ON CANVAS)
    begin

      Canvas.MoveTo((I * StepWidth) ,0);
      Canvas.LineTo((I * StepWidth) ,5);

       //STEPLINES
      for J := 0 to 9  do
      begin
        Canvas.MoveTo((ColWidth * J) + (StepWidth * (I - 1)) ,0);
        Canvas.LineTo((ColWidth * J) + (StepWidth * (I - 1)) ,3);
      end;

      //WRITE NUMBER
      if (i <> 0) and (i <> iMax)  then
        Canvas.TextOut(((I * StepWidth) - 3) ,3,inttostr((I * 10) + FOffset));

    end;
    SetBkMode(Canvas.Handle, OldBkMode);

    //REPAINT INTLIST
    for I := 0 to Columns.Count - 1 do
    begin
      PaintColumnLine(Columns[I] * ColWidth, I)
    end;
  end;
end;

constructor TAdvColumnSetter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColumns := TIntList.Create(-1,-1);
  FState := msNone;
  DoubleBuffered := true;
end;

procedure Register;
begin
  RegisterComponents('TMS Grids',[TAdvColumnSetter]);
end;

end.
