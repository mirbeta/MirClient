{***************************************************************************}
{ TChartLink component                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2011                                        }
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

{$I TMSDEFS.INC}

unit ChartLink;

interface

uses
  Classes, Series, TEEngine, AdvGrid, Graphics, Windows, Grids, SysUtils;

const  
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release

type
  TGridDataType = (dtCellRange, dtFullRow, dtFullColumn, dtNormalRow, dtNormalColumn, dtNode, dtNone);

  TGridData = class(TPersistent)
  private
    FLeft: Integer;
    FTop: Integer;
    FRight: Integer;
    FBottom: Integer;
    FDataType: TGridDataType;
    FColumn: Integer;
    FRow: Integer;
    FNodeLevel: Integer;
  published
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Right: Integer read FRight write FRight;
    property Bottom: Integer read FBottom write FBottom;
    property Column: Integer read FColumn write FColumn;
    property Row: Integer read FRow write FRow;
    property NodeLevel: Integer read FNodeLevel write FNodeLevel;
    property DataType: TGridDataType read FDataType write FDataType;
  end;

  TChartLink = class(TGridChangeNotifier)
  private
    FSeries: TChartSeries;
    FGrid: TAdvStringGrid;
    FGridValues: TGridData;
    FGridLegends: TGridData;
    FActive: Boolean;
    procedure SetGrid(const Value: TAdvStringGrid);
    procedure SetGridLegends(const Value: TGridData);
    procedure SetGridValues(const Value: TGridData);
    procedure SetActive(const Value: Boolean);
    function GetVersionEx: string;
    procedure SetVersion(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;  
    procedure CellsChanged(R:TRect); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Synchronize;
  published
    property Active: Boolean read FActive write SetActive;
    property Grid: TAdvStringGrid read FGrid write SetGrid;
    property GridValues: TGridData read FGridValues write SetGridValues;
    property GridLegends: TGridData read FGridLegends write SetGridLegends;
    property Series: TChartSeries read FSeries write FSeries;
    property Version: string read GetVersionEx write SetVersion;    
  end;

implementation

{ TChartLink }

procedure TChartLink.CellsChanged(R: TRect);
begin
  inherited;
  {Enhance to only synchronize when related cells changed}
  Synchronize;
end;

constructor TChartLink.Create(AOwner: TComponent);
begin
  inherited;
  FGridValues := TGridData.Create;
  FGridLegends := TGridData.Create;
end;

destructor TChartLink.Destroy;
begin
  FGridValues.Free;
  FGridLegends.Free;
  inherited;
end;

function TChartLink.GetVersionEx: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TChartLink.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TChartLink.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FGrid) then
   FGrid := nil;

  if (AOperation = opRemove) and (AComponent = FSeries) then
   FSeries := nil;

  inherited;
end;

procedure TChartLink.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then Synchronize;
  end;
end;

procedure TChartLink.SetGrid(const Value: TAdvStringGrid);
begin
  if FGrid <> Value then
  begin
    if not Assigned(Value) and Assigned(FGrid) then
      FGrid.UnRegisterNotifier(Self);

    if Assigned(Value) and not Assigned(FGrid) then
      Value.RegisterNotifier(Self);

    FGrid := Value;
  end;
end;

procedure TChartLink.SetGridLegends(const Value: TGridData);
begin
  FGridLegends := Value;
end;

procedure TChartLink.SetGridValues(const Value: TGridData);
begin
  FGridValues := Value;
end;

procedure TChartLink.SetVersion(const Value: string);
begin

end;

procedure TChartLink.Synchronize;
var
  Col,Row, Idx: Integer;
  SyncRect: TRect;
  Legend: string;
  CValue: Double;
  cg: TCellGraphic;
  addcell: Boolean;

begin
  if not Assigned(FGrid) then
    Exit;
  if not Assigned(Series) then
    Exit;
  if not Active then
    Exit;
  if csDesigning in ComponentState then
    Exit;

  Idx := 0;

  case FGridValues.DataType of
  dtCellRange:SyncRect := Rect(FGridValues.Left,FGridValues.Top,FGridValues.Right,FGridValues.Bottom);
  dtFullRow:
    begin
      SyncRect.Top := FGridValues.Row;
      SyncRect.Left := 0;
      SyncRect.Bottom := FGridValues.Row;
      SyncRect.Right := FGrid.ColCount - 1;
    end;
  dtFullColumn:
    begin
      SyncRect.Top := 0;
      SyncRect.Left := FGridValues.Column;
      SyncRect.Bottom := FGrid.RowCount - 1;
      SyncRect.Right := FGridValues.Column;
    end;
  dtNormalRow:
    begin
      SyncRect.Top := FGridValues.Row;
      SyncRect.Left := FGrid.FixedCols;
      SyncRect.Bottom := FGridValues.Row;
      SyncRect.Right := FGrid.ColCount - 1 - FGrid.FixedRightCols;
    end;
  dtNormalColumn:
    begin
      SyncRect.Top := FGrid.FixedRows;
      SyncRect.Left := FGridValues.Column;
      SyncRect.Bottom := FGrid.RowCount - 1 - FGrid.FixedFooters;
      SyncRect.Right := FGridValues.Column;
    end;
  dtNode:
    begin
      {
        dtNode is same as dtNormalColumn. The difference is in the action below
        after the 'case'
      }
      SyncRect.Top := FGrid.FixedRows;
      SyncRect.Left := FGridValues.Column;
      SyncRect.Bottom := FGrid.RowCount - 1 - FGrid.FixedFooters;
      SyncRect.Right := FGridValues.Column;
    end;
  end;

  if FGridValues.FDataType <> dtNone then
  begin
    for Col := SyncRect.Left to SyncRect.Right do
      for Row := SyncRect.Top to SyncRect.Bottom do
        begin
          addcell := true;

          if (FGridValues.FDataType = dtNode) then
          begin
            if FGrid.IsNode(Row) then
            begin
              if (FGrid.GetNodeLevel(Row) = FGridValues.FNodeLevel) then
              begin
                addcell := True;
              end;
            end
            else
              addcell := False;
          end;

          if addcell then
          begin          
            if Pos('=',FGrid.Cells[Col,Row]) = 1 then
            begin
              cg := FGrid.Objects[Col,Row] as TCellGraphic;
              if Assigned(cg) then
                CValue := cg.CellValue
              else
                CValue := 0;
            end
            else
            begin
              CValue := FGrid.Floats[Col,Row];
            end;

            if  Idx < Series.YValues.Count then
              Series.YValue[Idx] := CValue
            else
              Series.AddY(CValue,'');
            Inc(Idx);
          end;  
        end;
  end;

  while Idx < Series.YValues.Count do
    Series.YValues.Delete(Series.YValues.Count - 1);

  Idx := 0;

  case FGridLegends.FDataType of
  dtCellRange:SyncRect := Rect(FGridLegends.Left,FGridLegends.Top,FGridLegends.Right,FGridLegends.Bottom);
  dtFullRow:
    begin
      SyncRect.Top := FGridLegends.Row;
      SyncRect.Left := 0;
      SyncRect.Bottom := FGridLegends.Row;
      SyncRect.Right := FGrid.ColCount - 1;
    end;
  dtFullColumn:
    begin
      SyncRect.Top := 0;
      SyncRect.Left := FGridLegends.Column;
      SyncRect.Bottom := FGrid.RowCount - 1;
      SyncRect.Right := FGridLegends.Column;
    end;
  dtNormalRow:
    begin
      SyncRect.Top := FGridLegends.Row;
      SyncRect.Left := FGrid.FixedCols;
      SyncRect.Bottom := FGridLegends.Row;
      SyncRect.Right := FGrid.ColCount - 1 - FGrid.FixedRightCols;
    end;
  dtNormalColumn:
    begin
      SyncRect.Top := FGrid.FixedRows;
      SyncRect.Left := FGridLegends.Column;
      SyncRect.Bottom := FGrid.RowCount - 1 - FGrid.FixedFooters;
      SyncRect.Right := FGridLegends.Column;
    end;
  dtNode:
    begin
      SyncRect.Top := FGrid.FixedRows;
      SyncRect.Left := FGridLegends.Column;
      SyncRect.Bottom := FGrid.RowCount - 1 - FGrid.FixedFooters;
      SyncRect.Right := FGridLegends.Column;
    end;
  end;

  for Col := SyncRect.Left to SyncRect.Right do
    for Row := SyncRect.Top to SyncRect.Bottom do
      begin
        if FGridLegends.FDataType = dtNone then
        begin
          addcell := True;
          Legend := '';
        end
        else
          if (FGridValues.FDataType = dtNode) then
          begin
            if FGrid.IsNode(Row) then
            begin
                if (FGrid.GetNodeLevel(Row) = FGridValues.FNodeLevel) then
                begin
                  addcell := True;
                  Legend := FGrid.Cells[Col,Row];
                end
                else
                begin
                  addcell := False;
                end;
            end
            else
            begin
                addcell := False;
            end;
          end
          else
          begin
            addcell := True;
            Legend := FGrid.Cells[Col,Row];
          end;

        if (addCell) then
        begin
          if  (Idx < Series.XValues.Count) then
          begin
            Series.XLabel[Idx] := Legend;
          end
          else
          begin
            Series.AddX(0,Legend);
          end;
          Inc(Idx);
        end;  
      end;

  while Idx < Series.XValues.Count do
    Series.XValues.Delete(Series.XValues.Count - 1);
end;

end.
