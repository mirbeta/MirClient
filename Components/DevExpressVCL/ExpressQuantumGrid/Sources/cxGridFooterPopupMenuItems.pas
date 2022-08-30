{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid Utils                                 }
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

unit cxGridFooterPopupMenuItems;

{$I cxVer.inc}

interface

uses
  cxCustomData, cxGridTableView, cxGridMenuOperations;

type
  TcxGridFooterSummaryOperation = class(TcxGridTableColumnMenuOperation)
  private
    FKind: TcxSummaryKind;
    function GetFooterViewInfo: TcxGridFooterViewInfo;
    function GetMultipleSummaries: Boolean;
    function GetSummaryItems: TcxDataSummaryItems;
    procedure SetKind(Value: TcxSummaryKind);
  protected
    procedure Execute(Sender: TObject); override;
    function GetDown: Boolean; override;
    function GetEnabled: Boolean; override;
    function GetImageResourceName: string; override;
    property FooterViewInfo: TcxGridFooterViewInfo read GetFooterViewInfo;
    property MultipleSummaries: Boolean read GetMultipleSummaries;
    property SummaryItems: TcxDataSummaryItems read GetSummaryItems;
  public
    property Kind: TcxSummaryKind read FKind write SetKind;
  end;

  TcxGridFooterPopupMenuOperations = class(TcxGridPopupMenuOperations)
  protected
    procedure AddItems; override;
  end;

implementation

uses
  cxDataStorage, cxLookupEdit, cxGridCustomView,
  cxGridCustomPopupMenu, cxGridPopupMenu, cxGridPopupMenuConsts;

{ TcxGridFooterSummaryOperation }

function TcxGridFooterSummaryOperation.GetFooterViewInfo: TcxGridFooterViewInfo;
begin
  if GridPopupMenu.HitTest.ViewInfo is TcxGridFooterCellViewInfo then
    Result := TcxGridFooterCellViewInfo(GridPopupMenu.HitTest.ViewInfo).Container
  else
    if GridPopupMenu.HitTest.ViewInfo is TcxGridFooterViewInfo then
      Result := TcxGridFooterViewInfo(GridPopupMenu.HitTest.ViewInfo)
    else
      Result := nil;
end;

function TcxGridFooterSummaryOperation.GetMultipleSummaries: Boolean;
begin
  Result := FooterViewInfo.CanShowMultipleSummaries;
end;

function TcxGridFooterSummaryOperation.GetSummaryItems: TcxDataSummaryItems;
begin
  Result := FooterViewInfo.SummaryItems;
end;

procedure TcxGridFooterSummaryOperation.SetKind(Value: TcxSummaryKind);
begin
  FKind := Value;
  ResCaption := GetSummaryName(Kind);
  UpdateImage;
end;

procedure TcxGridFooterSummaryOperation.Execute(Sender: TObject);
var
  ASummaryItems: TcxDataSummaryItems;
  ASummaryItem: TcxDataSummaryItem;
begin
  ASummaryItems := SummaryItems;
  if MultipleSummaries then
    if Kind <> skNone then
      if Down then
        ASummaryItems.GetDataItem(HitColumn.Index, spFooter, True, Kind).Free
      else
        Params.Add(ASummaryItems.Add(HitColumn, spFooter, Kind))
    else
      if not Down then
        ASummaryItems.DeleteItems(HitColumn, spFooter)
      else
  else
  begin
    ASummaryItems.SetDataItemKind(HitColumn.Index, spFooter, Kind);
    ASummaryItem := ASummaryItems.GetDataItem(HitColumn.Index, spFooter, True, Kind);
    if ASummaryItem <> nil then Params.Add(ASummaryItem);
  end;
end;

function TcxGridFooterSummaryOperation.GetDown: Boolean;
var
  ASummaryItem: TcxDataSummaryItem;
begin
  if Kind = skNone then
    Result := SummaryItems.GetDataItem(HitColumn.Index, spFooter) = nil
  else
    if MultipleSummaries then
      Result := SummaryItems.GetDataItem(HitColumn.Index, spFooter, True, Kind) <> nil
    else
    begin
      ASummaryItem := SummaryItems.GetDataItem(HitColumn.Index, spFooter);
      Result := (ASummaryItem <> nil) and (ASummaryItem.Kind = Kind);
    end;
end;

function TcxGridFooterSummaryOperation.GetEnabled: Boolean;
begin
  Result := Kind in [skCount, skNone];
  if Result then Exit;

  Result := not HitColumn.GetProperties.InheritsFrom(TcxCustomLookupEditProperties);
  if not Result then Exit;

  Result := Kind in HitColumn.DataBinding.GetAllowedSummaryKinds;
end;

function TcxGridFooterSummaryOperation.GetImageResourceName: string;
begin
  Result := GetSummaryImageResourceName(Kind);
end;

{ TcxGridFooterPopupMenuOperations }

procedure TcxGridFooterPopupMenuOperations.AddItems;
var
  ASummaryKind: TcxSummaryKind;
begin
  for ASummaryKind := Low(ASummaryKind) to High(ASummaryKind) do
    if ASummaryKind <> skNone then
      TcxGridFooterSummaryOperation(AddItem(TcxGridFooterSummaryOperation)).Kind := ASummaryKind;
  with TcxGridFooterSummaryOperation(AddItem(TcxGridFooterSummaryOperation)) do
  begin
    BeginGroup := True;
    Kind := skNone;
  end;
end;

end.
