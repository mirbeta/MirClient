{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxGridGroupRowPopupMenuItems;

{$I cxVer.inc}

interface

uses
  dxCore, cxCustomData, cxGridCustomView, cxGridTableView, cxGridMenuOperations, Classes;

type
  TcxGridGroupRowPopupMenuOperation = class(TcxGridTablePopupMenuOperation)
  private
    function GetHitRow: TcxGridGroupRow;
  public
    property HitRow: TcxGridGroupRow read GetHitRow;
  end;

  TcxGridGroupRowSortByGroupValuesOperation = class(TcxGridGroupRowPopupMenuOperation)
  protected
    procedure Execute(Sender: TObject); override;
    function GetDown: Boolean; override;
  public
    constructor Create; override;
  end;

  TcxGridGroupRowSortBySummaryCaptionOperation = class(TcxGridGroupRowPopupMenuOperation)
  public
    constructor Create; override;
  end;

  TcxGridGroupRowSortBySummaryOperation = class(TcxGridGroupRowPopupMenuOperation)
  private
    FSummaryItem: TcxDataSummaryItem;
    procedure SetSummaryItem(Value: TcxDataSummaryItem);
  protected
    procedure Execute(Sender: TObject); override;
    function GetCaption: string; override;
    function GetDown: Boolean; override;
    function GetImageResourceName: string; override;
  public
    constructor Create; override;
    property SummaryItem: TcxDataSummaryItem read FSummaryItem write SetSummaryItem;
  end;

  TcxGridGroupRowPopupMenuOperations = class(TcxGridPopupMenuOperations)
  private
    FGroupRow: TcxGridGroupRow;
    procedure SetGroupRow(Value: TcxGridGroupRow);
  protected
    procedure AddItems; override;
  public
    function CanProcess(AHitTest: TcxCustomGridHitTest): Boolean; override;
    function HasDynamicContent: Boolean; override;

    property GroupRow: TcxGridGroupRow read FGroupRow write SetGroupRow;
  end;

implementation

uses
  SysUtils, cxClasses, cxGridCustomTableView, cxGridPopupMenuConsts;

{ TcxGridGroupRowPopupMenuOperation }

function TcxGridGroupRowPopupMenuOperation.GetHitRow: TcxGridGroupRow;
begin
  if (GridPopupMenu.HitTest is TcxGridRecordHitTest) and
    (TcxGridRecordHitTest(GridPopupMenu.HitTest).GridRecord is TcxGridGroupRow) then
    Result := TcxGridGroupRow(TcxGridRecordHitTest(GridPopupMenu.HitTest).GridRecord)
  else
    Result := nil;
end;

{ TcxGridGroupRowSortByGroupValuesOperation }

constructor TcxGridGroupRowSortByGroupValuesOperation.Create;
begin
  inherited;
  ResCaption := @cxSGridSortByGroupValues;
end;

procedure TcxGridGroupRowSortByGroupValuesOperation.Execute(Sender: TObject);
begin
  if HitRow.GroupSummaryItems.SortedSummaryItem <> nil then
    HitRow.GroupSummaryItems.SortedSummaryItem.Sorted := False;
end;

function TcxGridGroupRowSortByGroupValuesOperation.GetDown: Boolean;
begin
  Result := HitRow.GroupSummaryItems.SortedSummaryItem = nil;
end;

{ TcxGridGroupRowSortBySummaryCaptionOperation }

constructor TcxGridGroupRowSortBySummaryCaptionOperation.Create;
begin
  inherited;
  ResCaption := @cxSGridSortBySummaryCaption;
end;

{ TcxGridGroupRowSortBySummaryOperation }

constructor TcxGridGroupRowSortBySummaryOperation.Create;
begin
  inherited;
  ResCaption := @cxSGridSortBySummary;
end;

procedure TcxGridGroupRowSortBySummaryOperation.SetSummaryItem(Value: TcxDataSummaryItem);
begin
  if FSummaryItem <> Value then
  begin
    FSummaryItem := Value;
    UpdateImage;
  end;
end;

procedure TcxGridGroupRowSortBySummaryOperation.Execute(Sender: TObject);
begin
  SummaryItem.Sorted := True;
end;

function TcxGridGroupRowSortBySummaryOperation.GetCaption: string;
var
  AGridSummaryItem: IcxGridSummaryItem;
begin
  if Supports(SummaryItem, IcxGridSummaryItem, AGridSummaryItem) then
    Result := AGridSummaryItem.DisplayText
  else
    Result := '';
  if Result = '' then
  begin
    Result := cxGetResourceString(GetSummaryName(SummaryItem.Kind));
    if SummaryItem.ItemLink <> nil then
      Result := Format(inherited GetCaption,
        [Result, (SummaryItem.ItemLink as TcxGridColumn).GetAlternateCaption]);
  end;
  Result := '   ' + Result;
end;

function TcxGridGroupRowSortBySummaryOperation.GetDown: Boolean;
begin
  Result := SummaryItem.Sorted;
end;

function TcxGridGroupRowSortBySummaryOperation.GetImageResourceName: string;
begin
  if SummaryItem <> nil then
    Result := GetSummaryImageResourceName(SummaryItem.Kind)
  else
    Result := inherited GetImageResourceName;
end;

{ TcxGridGroupRowPopupMenuOperations }

procedure TcxGridGroupRowPopupMenuOperations.SetGroupRow(Value: TcxGridGroupRow);
begin
  FGroupRow := Value;
  RecreateItems;
end;

procedure TcxGridGroupRowPopupMenuOperations.AddItems;
var
  ASummaryItems: TcxDataGroupSummaryItems;
  I: Integer;
  AGridSummaryItem: IcxGridSummaryItem;
begin
  if GroupRow = nil then Exit;
  ASummaryItems := GroupRow.GroupSummaryItems;
  if ASummaryItems.Count = 0 then Exit;
  AddItem(TcxGridGroupRowSortByGroupValuesOperation);
  AddItem(TcxGridGroupRowSortBySummaryCaptionOperation).BeginGroup := True;
  for I := 0 to ASummaryItems.Count - 1 do
    if not Supports(ASummaryItems[I], IcxGridSummaryItem, AGridSummaryItem) or
      AGridSummaryItem.VisibleForCustomization then
      TcxGridGroupRowSortBySummaryOperation(AddItem(TcxGridGroupRowSortBySummaryOperation)).SummaryItem := ASummaryItems[I];
end;

function TcxGridGroupRowPopupMenuOperations.CanProcess(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := inherited CanProcess(AHitTest) and (AHitTest is TcxGridRecordHitTest) and
    not TcxGridRecordHitTest(AHitTest).GridRecord.IsData;
end;

function TcxGridGroupRowPopupMenuOperations.HasDynamicContent: Boolean;
begin
  Result := True;
end;

end.
