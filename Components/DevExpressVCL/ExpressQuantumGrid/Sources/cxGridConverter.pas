{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}
unit cxGridConverter;

{$I cxVer.inc}

interface

uses
  DesignIntf, Windows, SysUtils, Classes,
  dxCore, cxConverterFactory, cxGrid, cxGridCustomView, cxGridLevel, cxGridCustomTableView,
  cxGridDBBandedTableView, cxStyles, cxClasses, cxGridStrs, cxGridStructureNavigator,
  cxDesignWindows;

type
  TcxCustomGridConverterClass = class of TcxCustomGridConverter;

  { TcxCustomGridConverter }
  TcxCustomGridConverter = class(TcxCustomConverterWithStyles)
  private
    FDeleteAllSublevels: Boolean;
    FDestinationLevel: TcxGridLevel;
    FIntermediary: TComponent;
    procedure ClearAllSublevels(ALevel: TcxGridLevel);
    procedure ClearGrid;
    procedure ClearSublevels(ALevel: TcxGridLevel);
    procedure ClearViewItems(AView: TcxCustomGridView);
    function GetDestination: TcxCustomGrid;
    function GetDestinationLevel: TcxGridLevel;
    function GetcxGridView: TcxCustomGridView;
    function GetSource: TComponent;
    procedure SetIntermediary(AIntermediary: TComponent);
    procedure SetSource(ASource: TComponent);
  protected
    function CanConvert: Boolean; override;
    procedure DeleteComponent(AComponent: TComponent);
    procedure DoImport; override;
    procedure DoRealImport; virtual;
    function GetGridViewClass: TcxCustomGridViewClass; virtual;
    function TestIntermediary: Boolean; virtual;
    function UniqueColumnName(AColumn: TComponent; const AFieldName: string = ''): string;
    property cxGridView: TcxCustomGridView read GetcxGridView;
  public
    constructor Create(ADestination: TObject); override;
    class function GetIntermediaryClassName: string; virtual;
    property DeleteAllSublevels: Boolean read FDeleteAllSublevels write FDeleteAllSublevels;
    property Destination: TcxCustomGrid read GetDestination;
    property DestinationLevel: TcxGridLevel read FDestinationLevel write FDestinationLevel;
    property Intermediary: TComponent read FIntermediary write SetIntermediary;
    property Source: TComponent read GetSource write SetSource;
  end;

const
  cxGridGroupConverterName = 'Grid Converters';

implementation

uses
  cxGridCommon, cxGridBandedTableView;

{ TcxCustomGridConverter }

constructor TcxCustomGridConverter.Create(ADestination: TObject);
begin
  inherited Create(ADestination);
  FDeleteAllSublevels := True;
end;

class function TcxCustomGridConverter.GetIntermediaryClassName: string;
begin
  Result := '';
end;

function TcxCustomGridConverter.CanConvert: Boolean;
begin
  Result := TestIntermediary;
  if not Result  then
    MessageBox(0, PChar(Format(cxGetResourceString(@scxGridConverterIntermediaryMissing),
      [GetIntermediaryClassName])), PChar(cxGetResourceString(@scxImportErrorCaption)),
      MB_OK or MB_ICONINFORMATION);
end;

procedure TcxCustomGridConverter.DeleteComponent(AComponent: TComponent);
begin
  if Assigned(AComponent) and CanDeleteComponent(AComponent.Owner, AComponent) then
    AComponent.Free;
end;

procedure TcxCustomGridConverter.DoImport;
begin
  Destination.BeginUpdate;
  try
    ClearGrid;
    DoRealImport;
    inherited DoImport;
  finally
    Destination.EndUpdate;
  end;
end;

procedure TcxCustomGridConverter.DoRealImport;
begin
end;

function TcxCustomGridConverter.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBBandedTableView;
end;

function TcxCustomGridConverter.TestIntermediary: Boolean;
begin
  Result := True;
end;

//var
//  NCount: Integer = 0;

function TcxCustomGridConverter.UniqueColumnName(AColumn: TComponent; const AFieldName: string): string;
begin
//  Result := 'Column' + IntToStr(NCount);
//  Inc(NCount);
  Result := CreateUniqueName(AColumn.Owner, cxGridView, AColumn, ScxGridPrefixName, AFieldName);
end;

procedure TcxCustomGridConverter.ClearAllSublevels(ALevel: TcxGridLevel);
var
  I: Integer;
begin
  for I := ALevel.Count - 1 downto 0 do
    ClearSublevels(ALevel[I]);
end;

procedure TcxCustomGridConverter.ClearGrid;
var
  AOldView: TcxCustomGridView;
begin
  FDestinationLevel := GetDestinationLevel;
  AOldView := FDestinationLevel.GridView;
  if FDestinationLevel.Name = '' then
    FDestinationLevel.Name := GenLevelName(Destination, FDestinationLevel);
  if Assigned(AOldView) and (AOldView.ClassType = GetGridViewClass) then
  begin
    ClearViewItems(AOldView);
    Exit;
  end;
  FDestinationLevel.GridView := Destination.CreateView(GetGridViewClass);
  FDestinationLevel.GridView.Name := GenViewName(Destination, FDestinationLevel.GridView);
  DeleteComponent(AOldView);
end;

procedure TcxCustomGridConverter.ClearSublevels(ALevel: TcxGridLevel);
var
  I: Integer;
begin
  for I := ALevel.Count - 1 downto 0 do
    ClearSublevels(ALevel[I]);
  DeleteComponent(ALevel.GridView);
  DeleteComponent(ALevel);
end;

procedure TcxCustomGridConverter.ClearViewItems(AView: TcxCustomGridView);
var
  I: Integer;
begin
  if AView is TcxCustomGridTableView then
    with TcxCustomGridTableView(AView) do
    begin
      for I := ItemCount - 1 downto 0 do
        DeleteComponent(Items[I]);
    end;
end;

function TcxCustomGridConverter.GetDestination: TcxCustomGrid;
begin
  Result := inherited Destination as TcxCustomGrid;
end;

function TcxCustomGridConverter.GetDestinationLevel: TcxGridLevel;
begin
  Result := FDestinationLevel;
  if Result = nil then
  begin
    if FDeleteAllSublevels then
      ClearAllSublevels(Destination.Levels);
    Result := Destination.Levels.Add;
  end
  else
  begin
    if FDeleteAllSublevels then
      ClearAllSublevels(Result);
  end;
end;

function TcxCustomGridConverter.GetcxGridView: TcxCustomGridView;
begin
  Result := FDestinationLevel.GridView;
end;

function TcxCustomGridConverter.GetSource: TComponent;
begin
  Result := inherited Source as TComponent;
end;

procedure TcxCustomGridConverter.SetIntermediary(AIntermediary: TComponent);
begin
  if AIntermediary <> nil then
    if AIntermediary.ClassName = GetIntermediaryClassName then
      FIntermediary := AIntermediary;
end;

procedure TcxCustomGridConverter.SetSource(ASource: TComponent);
begin
  inherited Source := ASource;
end;

end.
