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

unit dxRichEdit.Dialogs.TableOptionsController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.PieceTable;

type
  TdxTableOptionsFormControllerParameters = class(TdxFormControllerParameters)
  private
    FTable: TdxTable;
  public
    constructor Create(const AControl: IdxRichEditControl; ATable: TdxTable);
    property Table: TdxTable read FTable;
  end;

  TdxTableOptionsFormController = class(TdxFormController)
  private
    FSourceTable: TdxTable;
    FLeftMargin: TdxNullableInteger;
    FRightMargin: TdxNullableInteger;
    FTopMargin: TdxNullableInteger;
    FBottomMargin: TdxNullableInteger;
    FCellSpacing: TdxNullableInteger;
    FAllowCellSpacing: TdxNullableBoolean;
    FResizeToFitContent: TdxNullableBoolean;
    function GetDocumentModel: TdxDocumentModel; inline;
  protected
    procedure ApplyCellSpacing;
    procedure ApplyCellSpacingCore;
    procedure ApplyMargins;
    function GetCellSpacing: TdxNullableInteger;
    procedure InitializeMargins;
    procedure InitializeController;
    procedure ResetCellSpacing;
    procedure ResetPropertiesException;

    class function EqualsWidthUnit(const AValue1, AValue2: TdxWidthUnit): Boolean; overload; static;
    class function EqualsWidthUnit(const AValue1: TdxWidthUnitInfo; const AValue2: TdxWidthUnit): Boolean; overload; static;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  public
    constructor Create(AControllerParameters: TdxTableOptionsFormControllerParameters);
    procedure ApplyChanges; override;

    property LeftMargin: TdxNullableInteger read FLeftMargin write FLeftMargin;
    property RightMargin: TdxNullableInteger read FRightMargin write FRightMargin;
    property TopMargin: TdxNullableInteger read FTopMargin write FTopMargin;
    property BottomMargin: TdxNullableInteger read FBottomMargin write FBottomMargin;
    property CellSpacing: TdxNullableInteger read FCellSpacing write FCellSpacing;
    property AllowCellSpacing: TdxNullableBoolean read FAllowCellSpacing write FAllowCellSpacing;
    property ResizeToFitContent: TdxNullableBoolean read FResizeToFitContent write FResizeToFitContent;
  end;

implementation

uses
  dxRichEdit.DocumentModel.Simple;

type
  TdxCellMargin = record
    FirstRowCellMargin: TdxMarginUnitBase;
    IdenticalMargins: Boolean;
    constructor Create(AFirstRowCellMargin: TdxMarginUnitBase; AIdenticalMargins: Boolean = True);
    procedure CheckIdenticalMargins(ACurrentMargins: TdxMarginUnitBase);
    class operator Implicit(const A: TdxCellMargin): TdxNullableInteger;
  end;

{ TdxCellMargins }

constructor TdxCellMargin.Create(AFirstRowCellMargin: TdxMarginUnitBase; AIdenticalMargins: Boolean);
begin
  FirstRowCellMargin := AFirstRowCellMargin;
  IdenticalMargins := AIdenticalMargins;
end;

procedure TdxCellMargin.CheckIdenticalMargins(ACurrentMargins: TdxMarginUnitBase);
begin
  IdenticalMargins := IdenticalMargins and TdxTableOptionsFormController.EqualsWidthUnit(FirstRowCellMargin, ACurrentMargins);
end;

class operator TdxCellMargin.Implicit(const A: TdxCellMargin): TdxNullableInteger;
begin
  if A.IdenticalMargins then
    Result := A.FirstRowCellMargin.Value
  else
    Result := TdxNullableInteger.Null;
end;

{ TdxTableOptionsFormControllerParameters }

constructor TdxTableOptionsFormControllerParameters.Create(const AControl: IdxRichEditControl; ATable: TdxTable);
begin
  inherited Create(AControl);
  FTable := ATable;
end;

{ TdxTableOptionsFormController }

procedure TdxTableOptionsFormController.ApplyCellSpacing;
begin
  if CellSpacing.IsNull then
    Exit;

  if (AllowCellSpacing = True) and (CellSpacing.Value <> 0) then
    ApplyCellSpacingCore
  else
    ResetCellSpacing;
end;

procedure TdxTableOptionsFormController.ApplyCellSpacingCore;
var
  ANewCellSpacing: TdxWidthUnitInfo;
  ATablePropertiesCellSpacing: TdxWidthUnit;
  ARows: TdxTableRowCollection;
  ARowPropertiesCellSpacing: TdxWidthUnit;
  ARowsCount: Integer;
  I: Integer;
begin
   ANewCellSpacing := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, CellSpacing.Value div 2);
   try
     ATablePropertiesCellSpacing := FSourceTable.TableProperties.CellSpacing;
     if not EqualsWidthUnit(ANewCellSpacing, ATablePropertiesCellSpacing) then
       ATablePropertiesCellSpacing.CopyFrom(ANewCellSpacing);

     ARows := FSourceTable.Rows;
     ARowsCount := ARows.Count;
     for I := 0 to ARowsCount - 1 do
     begin
       ARowPropertiesCellSpacing := ARows[i].Properties.CellSpacing;
       if not EqualsWidthUnit(ANewCellSpacing, ARowPropertiesCellSpacing) then
         ARowPropertiesCellSpacing.CopyFrom(ANewCellSpacing);
     end;
   finally
     ANewCellSpacing.Free;
   end;
end;

procedure TdxTableOptionsFormController.ApplyChanges;
begin
  DocumentModel.BeginUpdate;
  try
    ApplyMargins;
    if ResizeToFitContent = True then
      FSourceTable.TableLayout := TdxTableLayoutType.Autofit
    else
      FSourceTable.TableLayout := TdxTableLayoutType.Fixed;
    ApplyCellSpacing;
  finally
    DocumentModel.EndUpdate;
  end;
end;

constructor TdxTableOptionsFormController.Create(AControllerParameters: TdxTableOptionsFormControllerParameters);
begin
  inherited Create;
  FSourceTable := AControllerParameters.Table;
  InitializeController;
end;

class function TdxTableOptionsFormController.EqualsWidthUnit(const AValue1: TdxWidthUnitInfo;
  const AValue2: TdxWidthUnit): Boolean;
begin
  if (AValue1 = nil) or (AValue2 = nil) then
    Result := False
  else
    Result := (AValue1.&Type = AValue2.&Type) and (AValue1.Value = AValue2.Value);
end;

class function TdxTableOptionsFormController.EqualsWidthUnit(const AValue1, AValue2: TdxWidthUnit): Boolean;
begin
  Result := (AValue1.&Type = AValue2.&Type) and (AValue1.Value = AValue2.Value);
end;

function TdxTableOptionsFormController.GetCellSpacing: TdxNullableInteger;
var
  ARows: TdxTableRowCollection;
  ARowsCount: Integer;
  AFirstRowCellSpacing: TdxWidthUnit;
  ACurrentRow: TdxTableRow;
  I: Integer;
begin
  ARows := FSourceTable.Rows;
  ARowsCount := ARows.Count;

  AFirstRowCellSpacing := ARows.First.CellSpacing;
  for I := 0 to ARowsCount - 1 do
  begin
    ACurrentRow := ARows[i];
    if not EqualsWidthUnit(AFirstRowCellSpacing, ACurrentRow.CellSpacing) then
      Exit(TdxNullableInteger.Null);
  end;
  if AFirstRowCellSpacing.&Type = TdxWidthUnitType.ModelUnits then
    Result :=  AFirstRowCellSpacing.Value
  else
    Result := 0;
end;

function TdxTableOptionsFormController.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(FSourceTable.DocumentModel);
end;

procedure TdxTableOptionsFormController.ApplyMargins;

  procedure ApplyCellMargin(const AMargin: TdxNullableInteger; const AMarginUnit: TdxMarginUnitBase);
  var
    ANewMargin: TdxWidthUnitInfo;
  begin
    if AMargin.IsNull then
      Exit;
    ANewMargin := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, AMargin.Value);
    try
      if not EqualsWidthUnit(ANewMargin, AMarginUnit) then
        AMarginUnit.CopyFrom(ANewMargin);
    finally
      ANewMargin.Free;
    end;
  end;

var
  ASourceCellMargins: TdxCellMargins;
begin
  ASourceCellMargins := FSourceTable.TableProperties.CellMargins;
  ApplyCellMargin(LeftMargin, ASourceCellMargins.Left);
  ApplyCellMargin(RightMargin, ASourceCellMargins.Right);
  ApplyCellMargin(TopMargin, ASourceCellMargins.Top);
  ApplyCellMargin(BottomMargin, ASourceCellMargins.Bottom);
  ResetPropertiesException;
end;

procedure TdxTableOptionsFormController.InitializeController;
var
  ACellSpacing: TdxNullableInteger;
begin
  InitializeMargins;
  ACellSpacing := GetCellSpacing;
  if not ACellSpacing.IsNull then
    ACellSpacing.Value := ACellSpacing.Value * 2;
  CellSpacing := ACellSpacing;
  AllowCellSpacing := CellSpacing <> 0;
  ResizeToFitContent := FSourceTable.TableLayout = TdxTableLayoutType.Autofit;
end;

procedure TdxTableOptionsFormController.InitializeMargins;
var
  ARows: TdxTableRowCollection;
  ACurrentRow, AFirstRow: TdxTableRow;
  ALeftCellMargin, ARightCellMargin, ATopCellMargin, ABottomCellMargin: TdxCellMargin;
  ACurrentLeftMargins, ACurrentRightMargins, ACurrentTopMargins, ACurrentBottomMargins: TdxMarginUnitBase;
  ARowsCount: Integer;
  I: Integer;
begin
  ARows := FSourceTable.Rows;
  AFirstRow := ARows.First;

  ALeftCellMargin := TdxCellMargin.Create(AFirstRow.GetLeftCellMarginConsiderExceptions);
  ARightCellMargin := TdxCellMargin.Create(AFirstRow.GetRightCellMarginConsiderExceptions);
  ATopCellMargin := TdxCellMargin.Create(AFirstRow.GetTopCellMarginConsiderExceptions);
  ABottomCellMargin := TdxCellMargin.Create(AFirstRow.GetBottomCellMarginConsiderExceptions);

  ARowsCount := ARows.Count;
  for I := 0 to ARowsCount - 1 do
  begin
    ACurrentRow := ARows[I];
    ACurrentLeftMargins := ACurrentRow.GetLeftCellMarginConsiderExceptions;
    ALeftCellMargin.CheckIdenticalMargins(ACurrentLeftMargins);
    ACurrentRightMargins := ACurrentRow.GetRightCellMarginConsiderExceptions;
    ARightCellMargin.CheckIdenticalMargins(ACurrentRightMargins);
    ACurrentTopMargins := ACurrentRow.GetTopCellMarginConsiderExceptions;
    ATopCellMargin.CheckIdenticalMargins(ACurrentTopMargins);
    ACurrentBottomMargins := ACurrentRow.GetBottomCellMarginConsiderExceptions;
    ABottomCellMargin.CheckIdenticalMargins(ACurrentBottomMargins);
  end;
  LeftMargin := ALeftCellMargin;
  RightMargin := ARightCellMargin;
  TopMargin := ATopCellMargin;
  BottomMargin := ABottomCellMargin;
end;

procedure TdxTableOptionsFormController.ResetCellSpacing;
var
  ADefaultCellSpacing: TdxWidthUnit;
  ARows: TdxTableRowCollection;
  ARowsCount: Integer;
  ACurrentRowProperties: TdxTableRowProperties;
  I: Integer;
begin
  ADefaultCellSpacing := DocumentModel.DefaultTableProperties.CellSpacing;
  FSourceTable.TableProperties.CellSpacing.CopyFrom(ADefaultCellSpacing);
  FSourceTable.TableProperties.ResetUse(TdxTablePropertiesOptions.MaskUseCellSpacing);

  ARows := FSourceTable.Rows;
  ARowsCount := ARows.Count;
  for I := 0 to ARowsCount - 1 do
  begin
    ACurrentRowProperties := ARows[i].Properties;
    if ACurrentRowProperties.GetUse(TdxTableRowPropertiesOptions.MaskUseCellSpacing) then
    begin
      ACurrentRowProperties.CellSpacing.CopyFrom(ADefaultCellSpacing);
      ACurrentRowProperties.ResetUse(TdxTableRowPropertiesOptions.MaskUseCellSpacing);
    end;
  end;
end;

procedure TdxTableOptionsFormController.ResetPropertiesException;
var
  ADefaultCellMargins: TdxCellMargins;
  ARows: TdxTableRowCollection;
  APropertiesException: TdxTableProperties;
  ACellMargins: TdxCellMargins;
  ARowsCount: Integer;
  I: Integer;
begin
  ADefaultCellMargins := DocumentModel.DefaultTableProperties.CellMargins;
  ARows := FSourceTable.Rows;
  ARowsCount := ARows.Count;
  for I := 0 to ARowsCount - 1 do
  begin
    APropertiesException := ARows[i].TablePropertiesException;
    ACellMargins := APropertiesException.CellMargins;
    if not LeftMargin.IsNull and APropertiesException.GetUse(TdxTablePropertiesOptions.MaskUseLeftMargin) then
    begin
      ACellMargins.Left.CopyFrom(ADefaultCellMargins.Left);
      APropertiesException.ResetUse(TdxTablePropertiesOptions.MaskUseLeftMargin);
    end;
    if not RightMargin.IsNull and APropertiesException.GetUse(TdxTablePropertiesOptions.MaskUseRightMargin) then
    begin
      ACellMargins.Right.CopyFrom(ADefaultCellMargins.Right);
      APropertiesException.ResetUse(TdxTablePropertiesOptions.MaskUseRightMargin);
    end;
    if not TopMargin.IsNull and APropertiesException.GetUse(TdxTablePropertiesOptions.MaskUseTopMargin) then
    begin
      ACellMargins.Top.CopyFrom(ADefaultCellMargins.Top);
      APropertiesException.ResetUse(TdxTablePropertiesOptions.MaskUseTopMargin);
    end;
    if not BottomMargin.IsNull and APropertiesException.GetUse(TdxTablePropertiesOptions.MaskUseBottomMargin) then
    begin
      ACellMargins.Bottom.CopyFrom(ADefaultCellMargins.Bottom);
      APropertiesException.ResetUse(TdxTablePropertiesOptions.MaskUseBottomMargin);
    end;
  end;
end;

end.
