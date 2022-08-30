{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Dialogs.TableCellOptionsFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections, dxCoreClasses,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.TableFormatting;

type

  { TdxTableCellOptionsFormControllerParameters }

  TdxTableCellOptionsFormControllerParameters = class(TdxFormControllerParameters)
  private
    FSelectedCells: TdxTableCellList;
  public
    constructor Create(const AControl: IdxRichEditControl; ASelectedCells: TdxTableCellList);
    property SelectedCells: TdxTableCellList read FSelectedCells;
  end;

  { TdxTableCellOptionsFormController }

  TdxTableCellOptionsFormController = class(TdxFormController)
  private
    FRichEditControl: IdxRichEditControl;
    FSourceCells: TdxTableCellList;
    FCellMarginsSameAsTable: Boolean;
    FLeftMargin: TdxNullableInteger;
    FRightMargin: TdxNullableInteger;
    FTopMargin: TdxNullableInteger;
    FBottomMargin: TdxNullableInteger;
    FNoWrap: TdxNullableBoolean;
    FFitText: TdxNullableBoolean;
  protected
    procedure InitializeController;
    procedure InitializeCellMargins;
    class function EqualsWidthUnit(AValue1: TdxWidthUnit; AValue2: TdxWidthUnit): Boolean; overload;
    class function EqualsWidthUnit(AValue1: TdxWidthUnitInfo; AValue2: TdxWidthUnit): Boolean; overload;
    function IsIdenticalCellMarginsAsTableMargins: Boolean;
    function GetUseCellInnerMargin(AAccessor: TdxMarginUnitBase.TdxMarginPropertyAccessorBase; ACell: TdxTableCell): Boolean;
    function GetTotalNoWrap: TdxNullableBoolean;
    function GetTotalFitText: TdxNullableBoolean;
    procedure ApplyChangesCore(ADocumentModel: TdxDocumentModel);
    procedure ResetCellMargins(ADefaultCellMargins: TdxCellMargins; ACellMargins: TdxCellMargins; ACellProperties: TdxTableCellProperties);
    procedure ApplyCellMargins(ACell: TdxTableCell; ACellMargins: TdxCellMargins);
  public
    constructor Create(const AControllerParameters: TdxTableCellOptionsFormControllerParameters);
    procedure ApplyChanges; override;

    property RichEditControl: IdxRichEditControl read FRichEditControl;
    property CellMarginsSameAsTable: Boolean read FCellMarginsSameAsTable write FCellMarginsSameAsTable;
    property LeftMargin: TdxNullableInteger read FLeftMargin write FLeftMargin;
    property RightMargin: TdxNullableInteger read FRightMargin write FRightMargin;
    property TopMargin: TdxNullableInteger read FTopMargin write FTopMargin;
    property BottomMargin: TdxNullableInteger read FBottomMargin write FBottomMargin;
    property NoWrap: TdxNullableBoolean read FNoWrap write FNoWrap;
    property FitText: TdxNullableBoolean read FFitText write FFitText;
  end;

implementation

{ TdxTableCellOptionsFormControllerParameters }

constructor TdxTableCellOptionsFormControllerParameters.Create(const AControl: IdxRichEditControl;
  ASelectedCells: TdxTableCellList);
begin
  inherited Create(AControl);
  FSelectedCells := ASelectedCells;
end;

{ TdxTableCellOptionsFormController }

constructor TdxTableCellOptionsFormController.Create(
  const AControllerParameters: TdxTableCellOptionsFormControllerParameters);
begin
  inherited Create;
  FRichEditControl := AControllerParameters.Control;
  FSourceCells := AControllerParameters.SelectedCells;
  InitializeController;
end;

procedure TdxTableCellOptionsFormController.InitializeController;
begin
  InitializeCellMargins;
  CellMarginsSameAsTable := IsIdenticalCellMarginsAsTableMargins;

  NoWrap := GetTotalNoWrap;
  FitText := GetTotalFitText;
end;

procedure TdxTableCellOptionsFormController.InitializeCellMargins;
var
  AFirstCell, ACurrentCell: TdxTableCell;
  AFirstCellLeftMargin, AFirstCellRightMargin, AFirstCellTopMargin, AFirstCellBottomMargin: TdxMarginUnitBase;
  AIdenticalLeftMargins, AIdenticalRightMargins, AIdenticalTopMargins, AIdenticalBottomMargins: Boolean;
  ACellsCount, I: Integer;
begin
  AFirstCell := FSourceCells[0];

  AFirstCellLeftMargin := AFirstCell.GetActualLeftMargin;
  AFirstCellRightMargin := AFirstCell.GetActualRightMargin;
  AFirstCellTopMargin := AFirstCell.GetActualTopMargin;
  AFirstCellBottomMargin := AFirstCell.GetActualBottomMargin;

  AIdenticalLeftMargins := True;
  AIdenticalRightMargins := True;
  AIdenticalTopMargins := True;
  AIdenticalBottomMargins := True;

  ACellsCount := FSourceCells.Count;
  for I := 1 to ACellsCount - 1 do
  begin
    ACurrentCell := FSourceCells[I];
    AIdenticalLeftMargins := AIdenticalLeftMargins and EqualsWidthUnit(AFirstCellLeftMargin, ACurrentCell.GetActualLeftMargin);
    AIdenticalRightMargins := AIdenticalRightMargins and EqualsWidthUnit(AFirstCellRightMargin, ACurrentCell.GetActualRightMargin);
    AIdenticalTopMargins := AIdenticalTopMargins and EqualsWidthUnit(AFirstCellTopMargin, ACurrentCell.GetActualTopMargin);
    AIdenticalBottomMargins := AIdenticalBottomMargins and EqualsWidthUnit(AFirstCellBottomMargin, ACurrentCell.GetActualBottomMargin);
  end;

  LeftMargin := TdxNullableInteger.IfThen(AIdenticalLeftMargins, AFirstCellLeftMargin.Value);
  RightMargin := TdxNullableInteger.IfThen(AIdenticalRightMargins, AFirstCellRightMargin.Value);
  TopMargin := TdxNullableInteger.IfThen(AIdenticalTopMargins, AFirstCellTopMargin.Value);
  BottomMargin := TdxNullableInteger.IfThen(AIdenticalBottomMargins, AFirstCellBottomMargin.Value);
end;

class function TdxTableCellOptionsFormController.EqualsWidthUnit(AValue1: TdxWidthUnit; AValue2: TdxWidthUnit): Boolean;
begin
  if (AValue1 = nil) and (AValue2 = nil) then
    Exit(True);
  if (AValue1 = nil) or (AValue2 = nil) then
    Result := False
  else
    Result := (AValue1.&Type = AValue2.&Type) and (AValue1.Value = AValue2.Value);
end;

function TdxTableCellOptionsFormController.IsIdenticalCellMarginsAsTableMargins: Boolean;
var
  ACellsCount, I: Integer;
  ACurrentCell: TdxTableCell;
  AUseLeftInnerMargin, AUseRightInnerMargin, AUseTopInnerMargin, AUseBottomInnerMargin: Boolean;
begin
  ACellsCount := FSourceCells.Count;
  for I := 0 to ACellsCount - 1 do
  begin
    ACurrentCell := FSourceCells[I];
    AUseLeftInnerMargin := GetUseCellInnerMargin(TdxLeftMarginUnit.PropertyAccessor, ACurrentCell);
    AUseRightInnerMargin := GetUseCellInnerMargin(TdxRightMarginUnit.PropertyAccessor, ACurrentCell);
    AUseTopInnerMargin := GetUseCellInnerMargin(TdxTopMarginUnit.PropertyAccessor, ACurrentCell);
    AUseBottomInnerMargin := GetUseCellInnerMargin(TdxBottomMarginUnit.PropertyAccessor, ACurrentCell);

    if AUseLeftInnerMargin or AUseRightInnerMargin or AUseTopInnerMargin or AUseBottomInnerMargin then
      Exit(False);
  end;
  Result := True;
end;

function TdxTableCellOptionsFormController.GetUseCellInnerMargin(AAccessor: TdxMarginUnitBase.TdxMarginPropertyAccessorBase;
  ACell: TdxTableCell): Boolean;
var
  ACellProperties, ATableStyleCellProperties: TdxTableCellProperties;
begin
  ACellProperties := ACell.Properties;
  if ACellProperties.GetUse(AAccessor.CellPropertiesMask) then
    Exit(True);
  ATableStyleCellProperties := ACell.Table.TableStyle.TableCellProperties;
  Result := ATableStyleCellProperties.GetUse(AAccessor.CellPropertiesMask);
end;

function TdxTableCellOptionsFormController.GetTotalNoWrap: TdxNullableBoolean;
var
  I: Integer;
  AFirstCellNoWrap: Boolean;
  ACurrentCell: TdxTableCell;
begin
  AFirstCellNoWrap := FSourceCells[0].NoWrap;
  for I := 1 to FSourceCells.Count - 1 do
  begin
    ACurrentCell := FSourceCells[I];
    if AFirstCellNoWrap <> ACurrentCell.NoWrap then
      Exit(TdxNullableBoolean.Null);
  end;
  Result.Value := AFirstCellNoWrap;
end;

function TdxTableCellOptionsFormController.GetTotalFitText: TdxNullableBoolean;
var
  I: Integer;
  AFirstCellFitText: Boolean;
  ACurrentCell: TdxTableCell;
begin
  AFirstCellFitText := FSourceCells[0].FitText;
  for I := 1 to FSourceCells.Count - 1 do
  begin
    ACurrentCell := FSourceCells[I];
    if AFirstCellFitText <> ACurrentCell.FitText then
      Exit(TdxNullableBoolean.Null);
  end;
  Result.Value := AFirstCellFitText;
end;

procedure TdxTableCellOptionsFormController.ApplyChanges;
var
  ADocumentModel: TdxDocumentModel;
begin
  ADocumentModel := TdxDocumentModel(FSourceCells[0].DocumentModel);
  ADocumentModel.BeginUpdate;
  try
    ApplyChangesCore(ADocumentModel);
  finally
    ADocumentModel.EndUpdate;
  end;
end;

procedure TdxTableCellOptionsFormController.ApplyChangesCore(ADocumentModel: TdxDocumentModel);
var
  ADefaultCellMargins, ACurrentCellMargins: TdxCellMargins;
  ACellsCount, I: Integer;
  ACurrentCell: TdxTableCell;
  ACurrentCellProperties: TdxTableCellProperties;
begin
  ADefaultCellMargins := ADocumentModel.DefaultTableCellProperties.CellMargins;
  ACellsCount := FSourceCells.Count;
  for I := 0 to ACellsCount - 1 do
  begin
    ACurrentCell := FSourceCells[I];
    ACurrentCellProperties := ACurrentCell.Properties;
    if not NoWrap.IsNull and (ACurrentCellProperties.NoWrap <> NoWrap.Value) then
      ACurrentCellProperties.NoWrap := NoWrap.Value;
    if not FitText.IsNull and (ACurrentCellProperties.FitText <> FitText.Value) then
      ACurrentCellProperties.FitText := FitText.Value;

    ACurrentCellMargins := ACurrentCellProperties.CellMargins;
    if CellMarginsSameAsTable then
      ResetCellMargins(ADefaultCellMargins, ACurrentCellMargins, ACurrentCellProperties)
    else
      ApplyCellMargins(ACurrentCell, ACurrentCellMargins);
  end;
end;

procedure TdxTableCellOptionsFormController.ResetCellMargins(ADefaultCellMargins: TdxCellMargins; ACellMargins: TdxCellMargins;
  ACellProperties: TdxTableCellProperties);
begin
  ACellMargins.Left.CopyFrom(ADefaultCellMargins.Left);
  ACellMargins.Right.CopyFrom(ADefaultCellMargins.Right);
  ACellMargins.Top.CopyFrom(ADefaultCellMargins.Top);
  ACellMargins.Bottom.CopyFrom(ADefaultCellMargins.Bottom);

  ACellProperties.ResetUse(TdxTableCellPropertiesOptions.MaskUseLeftMargin);
  ACellProperties.ResetUse(TdxTableCellPropertiesOptions.MaskUseRightMargin);
  ACellProperties.ResetUse(TdxTableCellPropertiesOptions.MaskUseTopMargin);
  ACellProperties.ResetUse(TdxTableCellPropertiesOptions.MaskUseBottomMargin);
end;

procedure TdxTableCellOptionsFormController.ApplyCellMargins(ACell: TdxTableCell; ACellMargins: TdxCellMargins);
var
  ANewLeftMargin, ANewRightMargin, ANewTopMargin, ANewBottomMargin: TdxWidthUnitInfo;
begin
  if not LeftMargin.IsNull then
  begin
    ANewLeftMargin := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, LeftMargin.Value);
    try
      if not EqualsWidthUnit(ANewLeftMargin, ACell.GetActualLeftMargin) then
        ACellMargins.Left.CopyFrom(ANewLeftMargin);
    finally
      ANewLeftMargin.Free;
    end;
  end;
  if not RightMargin.IsNull then
  begin
    ANewRightMargin := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, RightMargin.Value);
    try
      if not EqualsWidthUnit(ANewRightMargin, ACell.GetActualRightMargin) then
        ACellMargins.Right.CopyFrom(ANewRightMargin);
    finally
      ANewRightMargin.Free;
    end;
  end;
  if not TopMargin.IsNull then
  begin
    ANewTopMargin := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, TopMargin.Value);
    try
      if not EqualsWidthUnit(ANewTopMargin, ACell.GetActualTopMargin) then
        ACellMargins.Top.CopyFrom(ANewTopMargin);
    finally
      ANewTopMargin.Free;
    end;
  end;
  if not BottomMargin.IsNull then
  begin
    ANewBottomMargin := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, BottomMargin.Value);
    try
      if not EqualsWidthUnit(ANewBottomMargin, ACell.GetActualBottomMargin) then
        ACellMargins.Bottom.CopyFrom(ANewBottomMargin);
    finally
      ANewBottomMargin.Free;
    end;
  end;
end;

class function TdxTableCellOptionsFormController.EqualsWidthUnit(AValue1: TdxWidthUnitInfo; AValue2: TdxWidthUnit): Boolean;
begin
  if (AValue1 = nil) or (AValue2 = nil) then
    Result := False
  else
    Result := (AValue1.&Type = AValue2.&Type) and (AValue1.Value = AValue2.Value);
end;

end.
