{***************************************************************************}
{ TAdvGridLookupBar component                                               }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2011 - 2014                                        }
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

unit AdvGridLookupBar;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvLookupBar, AdvGrid, SysUtils, Windows;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGridLookupBar = class(TAdvLookupBar, ITAdvStringGridSelect)
  private
    FGrid: TAdvStringGrid;
    FColumn: integer;
    FSelChange: boolean;
    procedure SetColumn(const Value: integer);
    procedure SetGrid(const Value: TAdvStringGrid);
  protected
    procedure SelectionChange(Col,Row: integer);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DoLookupClick(ACurrentChar: TCharRec); override;
    procedure DoLookup(ACurrentChar: TCharRec); override;
    procedure DoSyncGrid(Row: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Grid: TAdvStringGrid read FGrid write SetGrid;
    property Column: integer read FColumn write SetColumn default -1;
  end;

implementation

{ TAdvGridLookupBar }

constructor TAdvGridLookupBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColumn := -1;
end;

destructor TAdvGridLookupBar.Destroy;
begin
  if Assigned(FGrid) then
    FGrid.UnRegisterSelListener(self);
  inherited;
end;

procedure TAdvGridLookupBar.DoLookup(ACurrentChar: TCharRec);
begin
  inherited;
  if FSelChange then
    Exit;

  DoSyncGrid(ACurrentChar.Tag);
end;

procedure TAdvGridLookupBar.DoLookupClick(ACurrentChar: TCharRec);
begin
  inherited;
  DoSyncGrid(ACurrentChar.Tag);
end;

procedure TAdvGridLookupBar.DoSyncGrid(Row: integer);
begin
  if Assigned(FGrid) then
  begin
    ProgLookup := true;
    try
      if Categories.Count > 0 then
        FGrid.TopRow := Row
      else
        FGrid.TopRow := Row + FGrid.FixedRows;

      FGrid.Row := FGrid.TopRow;
      FGrid.Col := FColumn;
    finally
      ProgLookup := false;
    end;
  end;
end;

procedure TAdvGridLookupBar.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FGrid) then
  begin
    FGrid := nil;
  end;

  inherited;
end;

procedure TAdvGridLookupBar.SelectionChange(Col, Row: integer);
var
  s: string;
begin
  if not ProgLookup and (FColumn >= 0) then
  begin
    FSelChange := true;
    s := FGrid.Cells[FColumn,Row];
    if (s <> '') then
      SelectLookup(s[1]);
    FSelChange := false;
  end;
end;

procedure TAdvGridLookupBar.SetColumn(const Value: integer);
var
  sl: TStringList;
  i: integer;
begin
  if Assigned(FGrid) then
  begin
    if (Value >= 0) and (Value < FGrid.ColCount) then
    begin
      FColumn := Value;

      if (FGrid.FixedRows > 0) or (FGrid.FixedFooters > 0) then
      begin
        sl := TStringList.Create;
        sl.Assign(FGrid.Cols[FColumn]);

        for i := 0 to FGrid.FixedRows - 1 do
          sl.Delete(0);

        for i := 0 to FGrid.FixedFooters - 1 do
          sl.Delete(sl.Count - 1);

        InitLookupBar(sl);
        sl.Free;
      end
      else
      begin
        InitLookupBar(FGrid.Cols[FColumn]);
      end;

      SelectionChange(FColumn, FGrid.Row);
    end;
  end;
end;

procedure TAdvGridLookupBar.SetGrid(const Value: TAdvStringGrid);
begin
  if Assigned(FGrid) then
    FGrid.UnRegisterSelListener(self);

  FGrid := Value;

  if Assigned(FGrid) then
  begin
    FGrid.RegisterSelListener(self);

    // auto position lookupbar left or right from grid
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      if Left > FGrid.Left then
        Left := FGrid.Left + FGrid.Width - 2
      else
        Left := FGrid.Left - Width + 2;

      Top := FGrid.Top + 1;
      Height := FGrid.Height - 2;
    end;
  end;
end;

end.
