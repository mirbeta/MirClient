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

unit dxRichEdit.Dialogs.BorderShadingController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Graphics, Generics.Defaults, Generics.Collections, dxCoreGraphics, dxCoreClasses,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Commands.Tables.Cells;

type
  TdxBorderArray = array of TdxBorderInfo;

  TdxSetMode = (
    None,
    Box,
    All,
    Grid,
    Custom
  );

  { TdxBorderShadingFormControllerParameters }

  TdxBorderShadingFormControllerParameters = class(TdxFormControllerParameters)
  private
    FSelectedCells: TdxSelectedCellsCollection;
    FDocumentModel: TdxDocumentModel;
  public
    constructor Create(const AControl: IdxRichEditControl; ADocumentModel: TdxDocumentModel; ASelectedCells: TdxSelectedCellsCollection);
    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property SelectedCells: TdxSelectedCellsCollection read FSelectedCells;
  end;

  { TdxBorderShadingFormController }

  TdxBorderShadingFormController = class(TdxFormController)
  private
    FSelectedCells: TdxSelectedCellsCollection;
    FDocumentModel: TdxDocumentModel;
    FRichEditControl: IdxRichEditControl;
    FGridWidth: Integer;
    FSetModeButton: TdxSetMode;
    FBorderLineUp: TdxBorderInfo;
    FBorderLineVerticalIn: TdxBorderInfo;
    FBorderLineLeft: TdxBorderInfo;
    FBorderLineRight: TdxBorderInfo;
    FBorderLineHorizontalIn: TdxBorderInfo;
    FBorderLineDown: TdxBorderInfo;
    FBorderLineHorizontalInVisible: Boolean;
    FDrawParagraph: Boolean;
    FBorderLineVerticalInVisible: Boolean;
    FDrawColumns: Boolean;
    FFillColor: TdxNullableValue<TdxAlphaColor>;
  protected
    function GetModeState: TdxSetMode;
    procedure SetBorders(ACommandClass: TdxToggleTableCellsBordersCommandBaseClass; ABorder: TdxBorderInfo);
    procedure SetFillColor;
    procedure InitializeController;
    function IsModeStateAll: Boolean;
    function IsModeStateBox: Boolean;
    function IsModeStateGrid: Boolean;
    function IsModeStateNone: Boolean;

    class function AreEqual(ABorderFirst, ABorderSecond: TdxBorderInfo): Boolean; static;
  public
    constructor Create(AControllerParameters: TdxBorderShadingFormControllerParameters);
    procedure ApplyChanges; override;
    function GetActiveColor: TdxAlphaColor;
    function GetInitialBorder: TdxBorderInfo;

    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property RichEditControl: IdxRichEditControl read FRichEditControl;
    property SetModeButton: TdxSetMode read FSetModeButton write FSetModeButton;

    property BorderLineUp: TdxBorderInfo read FBorderLineUp write FBorderLineUp;
    property BorderLineHorizontalIn: TdxBorderInfo read FBorderLineHorizontalIn write FBorderLineHorizontalIn;
    property BorderLineDown: TdxBorderInfo read FBorderLineDown write FBorderLineDown;
    property BorderLineLeft: TdxBorderInfo read FBorderLineLeft write FBorderLineLeft;
    property BorderLineRight: TdxBorderInfo read FBorderLineRight write FBorderLineRight;
    property BorderLineVerticalIn: TdxBorderInfo read FBorderLineVerticalIn write FBorderLineVerticalIn;
    property BorderLineHorizontalInVisible: Boolean read FBorderLineHorizontalInVisible write FBorderLineHorizontalInVisible;
    property BorderLineVerticalInVisible: Boolean read FBorderLineVerticalInVisible write FBorderLineVerticalInVisible;
    property DrawColumns: Boolean read FDrawColumns write FDrawColumns;
    property DrawParagraph: Boolean read FDrawParagraph write FDrawParagraph;

    property FillColor: TdxNullableValue<TdxAlphaColor> read FFillColor write FFillColor;
  end;

implementation

{ TdxBorderShadingFormControllerParameters }

constructor TdxBorderShadingFormControllerParameters.Create(const AControl: IdxRichEditControl;
  ADocumentModel: TdxDocumentModel; ASelectedCells: TdxSelectedCellsCollection);
begin
  inherited Create(AControl);
  FSelectedCells := ASelectedCells;
  FDocumentModel := ADocumentModel;
end;

{ TdxBorderShadingFormController }

procedure TdxBorderShadingFormController.ApplyChanges;
begin
  DocumentModel.BeginUpdate();
  try
    if Assigned(BorderLineUp) then
      SetBorders(TdxToggleTableCellsTopBorderCommand, BorderLineUp);
    if Assigned(BorderLineDown) then
      SetBorders(TdxToggleTableCellsBottomBorderCommand, BorderLineDown);
    if Assigned(BorderLineHorizontalIn) then
      SetBorders(TdxToggleTableCellsInsideHorizontalBorderCommand, BorderLineHorizontalIn);
    if Assigned(BorderLineLeft) then
      SetBorders(TdxToggleTableCellsLeftBorderCommand, BorderLineLeft);
    if Assigned(BorderLineRight) then
      SetBorders(TdxToggleTableCellsRightBorderCommand, BorderLineRight);
    if Assigned(BorderLineVerticalIn) then
      SetBorders(TdxToggleTableCellsInsideVerticalBorderCommand, BorderLineVerticalIn);
    if not FillColor.IsNull then
      SetFillColor;
  finally
    DocumentModel.EndUpdate;
  end;
end;

class function TdxBorderShadingFormController.AreEqual(ABorderFirst, ABorderSecond: TdxBorderInfo): Boolean;
begin
  Result := (ABorderFirst.Style = ABorderSecond.Style) and (ABorderFirst.Color = ABorderSecond.Color) and (ABorderFirst.Width = ABorderSecond.Width);
end;

constructor TdxBorderShadingFormController.Create(AControllerParameters: TdxBorderShadingFormControllerParameters);
begin
  inherited Create;
  FSelectedCells := AControllerParameters.SelectedCells;
  FDocumentModel := AControllerParameters.DocumentModel;
  FRichEditControl := AControllerParameters.Control;
  InitializeController;
end;

function TdxBorderShadingFormController.GetActiveColor: TdxAlphaColor;
begin
  if FillColor.IsNull then
    Result := TdxAlphaColors.Empty
  else
    Result := FillColor.Value;
end;

function TdxBorderShadingFormController.GetInitialBorder: TdxBorderInfo;
var
  ABorders: TdxBorderArray;
  I: Integer;
begin
  ABorders := TdxBorderArray.Create(BorderLineUp, BorderLineRight, BorderLineDown, BorderLineLeft, BorderLineVerticalIn, BorderLineHorizontalIn);
  for I := Low(ABorders) to High(ABorders) do
    if Assigned(ABorders[i]) and (ABorders[i].Style <> TdxBorderLineStyle.None) then
      Exit(ABorders[I]);
  Result := DocumentModel.TableBorderInfoRepository.CurrentItem;
end;

function TdxBorderShadingFormController.GetModeState: TdxSetMode;
begin
  if IsModeStateNone then
    Result := TdxSetMode.None
  else
    if IsModeStateAll then
      Result := TdxSetMode.All
    else
      if IsModeStateBox then
        Result := TdxSetMode.Box
      else
        if IsModeStateGrid then
          Result := TdxSetMode.Grid
        else
          Result := TdxSetMode.Custom;
end;

procedure TdxBorderShadingFormController.InitializeController;
var
  AFirstInterval, ALastInterval, ASelectedCell: TdxSelectedCellsIntervalInRow;
  I, J, AStartIndex, AEndIndex: Integer;
  ARowsCount: Integer;
begin
  AFirstInterval := FSelectedCells.NormalizedFirst;
  ALastInterval := FSelectedCells.NormalizedLast;
  BorderLineUp := AFirstInterval.StartCell.GetActualTopCellBorder.Info;
  BorderLineDown := ALastInterval.StartCell.GetActualBottomCellBorder.Info;
  BorderLineHorizontalIn := AFirstInterval.StartCell.GetActualBottomCellBorder.Info;
  BorderLineLeft := AFirstInterval.StartCell.GetActualLeftCellBorder.Info;
  BorderLineRight := AFirstInterval.EndCell.GetActualRightCellBorder.Info;
  BorderLineVerticalIn := AFirstInterval.StartCell.GetActualRightCellBorder.Info;

  ARowsCount := FSelectedCells.RowsCount;
  for J := 0 to ARowsCount - 1 do
    if (FSelectedCells[J].NormalizedEndCellIndex - FSelectedCells[J].NormalizedStartCellIndex + 1) >= 2 then
    begin
      BorderLineVerticalInVisible := True;
      Break;
    end;

  BorderLineHorizontalInVisible := ARowsCount >= 2;

  FillColor := AFirstInterval.StartCell.GetActualBackgroundColor;

  for J := 0 to ARowsCount - 1 do
  begin
    ASelectedCell := FSelectedCells[J];
    for I := ASelectedCell.NormalizedStartCellIndex to ASelectedCell.NormalizedEndCellIndex do
      if (ASelectedCell.Row.Cells[i].GetActualBackgroundColor <> FillColor) then
      begin
        FillColor := TdxNullableValue<TdxAlphaColor>.Null;
        Break;
      end;
  end;
  for I := AFirstInterval.NormalizedStartCellIndex to AFirstInterval.NormalizedEndCellIndex do
    if not AFirstInterval.Row.Cells[I].GetActualTopCellBorder.Info.Equals(BorderLineUp) then
    begin
      BorderLineUp := nil;
      Break;
    end;
  for I := ALastInterval.NormalizedStartCellIndex to ALastInterval.NormalizedEndCellIndex do
    if not ALastInterval.Row.Cells[I].GetActualBottomCellBorder.Info.Equals(BorderLineDown) then
    begin
      BorderLineDown := nil;
      Break;
    end;
  if ARowsCount >= 2 then
  begin
    if AFirstInterval = FSelectedCells.First then
    begin
      AStartIndex := 0;
      AEndIndex := ARowsCount - 2;
    end
    else
    begin
      AStartIndex := 1;
      AEndIndex := ARowsCount - 1;
    end;
    for J := AStartIndex to AEndIndex do
    begin
      ASelectedCell := FSelectedCells[J];
      for I := ASelectedCell.NormalizedStartCellIndex to ASelectedCell.NormalizedEndCellIndex do
        if not ASelectedCell.Row.Cells[I].GetActualBottomCellBorder.Info.Equals(BorderLineHorizontalIn) then
        begin
          BorderLineHorizontalIn := nil;
          Break;
        end;
    end;
  end;
  for I := 0 to ARowsCount - 1 do
    if not FSelectedCells[I].StartCell.GetActualLeftCellBorder.Info.Equals(BorderLineLeft) then
    begin
      BorderLineLeft := nil;
      Break;
    end;
  for I := 0 to ARowsCount - 1 do
    if not FSelectedCells[I].EndCell.GetActualRightCellBorder.Info.Equals(BorderLineRight) then
    begin
      BorderLineRight := nil;
      Break;
    end;
  for J := 0 to ARowsCount - 1 do
  begin
    ASelectedCell := FSelectedCells[J];
    for I := ASelectedCell.NormalizedStartCellIndex to ASelectedCell.NormalizedEndCellIndex - 1 do
      if not ASelectedCell.Row.Cells[I].GetActualRightCellBorder.Info.Equals(BorderLineVerticalIn) then
      begin
        BorderLineVerticalIn := nil;
        Break;
      end;
  end;
  FGridWidth := DocumentModel.UnitConverter.TwipsToModelUnits(15);
  SetModeButton := GetModeState;
end;

function TdxBorderShadingFormController.IsModeStateAll: Boolean;
var
  ABorders: TdxBorderArray;
  I: Integer;
begin
  ABorders := TdxBorderArray.Create(BorderLineUp, BorderLineRight, BorderLineDown, BorderLineLeft, BorderLineVerticalIn, BorderLineHorizontalIn);
  if (ABorders[0] = nil) or (ABorders[0].Style = TdxBorderLineStyle.None) then
    Exit(False);
  if (not BorderLineVerticalInVisible) and (not BorderLineHorizontalInVisible) then
    Exit(False);
  for I := 1 to Length(ABorders) - 1 do
    if (ABorders[I] = nil) or not AreEqual(ABorders[0], ABorders[i]) then
      Exit(False);
  Result := True;
end;

function TdxBorderShadingFormController.IsModeStateBox: Boolean;
var
  ABorders, ABordersIn: TdxBorderArray;
  ABorder: TdxBorderInfo;
  I: Integer;
begin
  ABorders := TdxBorderArray.Create(BorderLineUp, BorderLineRight, BorderLineDown, BorderLineLeft);
  ABordersIn := TdxBorderArray.Create(BorderLineVerticalIn, BorderLineHorizontalIn);
  if (ABorders[0] = nil) or (ABorders[0].Style = TdxBorderLineStyle.None) then
    Exit(False);
  for I := 1 to Length(ABorders) - 1 do
    if (ABorders[I] = nil) or not AreEqual(ABorders[0], ABorders[i]) then
      Exit(False);
  if (not BorderLineVerticalInVisible) and (not BorderLineHorizontalInVisible) then
    Exit(True);
  for ABorder in ABordersIn do
    if (ABorder = nil) or (ABorder.Style <> TdxBorderLineStyle.None) then
      Exit(False);
  Result := True;
end;

function TdxBorderShadingFormController.IsModeStateGrid: Boolean;
var
  ABorders, ABordersIn: TdxBorderArray;
  ABorder: TdxBorderInfo;
  I: Integer;
begin
  ABorders := TdxBorderArray.Create(BorderLineUp, BorderLineRight, BorderLineDown, BorderLineLeft);
  ABordersIn := TdxBorderArray.Create(BorderLineVerticalIn, BorderLineHorizontalIn);
  if (ABorders[0] = nil) or (ABorders[0].Style = TdxBorderLineStyle.None) then
    Exit(False);
  for I := 1 to Length(ABorders) - 1 do
    if (ABorders[I] = nil) or not AreEqual(ABorders[0], ABorders[i]) then
      Exit(False);
  if (not BorderLineVerticalInVisible) and (not BorderLineHorizontalInVisible) then
    Exit(False);
  for ABorder in ABordersIn do
    if (ABorder = nil) or (ABorder.Style <> TdxBorderLineStyle.Single) or (ABorder.Color <> ABorders[0].Color) or (ABorder.Width <> FGridWidth) then
      Exit(False);
  Result := True;
end;

function TdxBorderShadingFormController.IsModeStateNone: Boolean;
var
  ABorders: TdxBorderArray;
  ABorder: TdxBorderInfo;
begin
  ABorders := TdxBorderArray.Create(BorderLineUp, BorderLineRight, BorderLineDown, BorderLineLeft, BorderLineVerticalIn, BorderLineHorizontalIn);
  for ABorder in ABorders do
    if Assigned(ABorder) and (ABorder.Style <> TdxBorderLineStyle.None) then
      Exit(False);
  Result := True;
end;

procedure TdxBorderShadingFormController.SetBorders(ACommandClass: TdxToggleTableCellsBordersCommandBaseClass;
  ABorder: TdxBorderInfo);
var
  ACommand: TdxToggleTableCellsBordersCommandBase;
  AState: IdxCommandUIState;
begin
  ACommand := ACommandClass.Create(RichEditControl);
  try
    ACommand.NewBorder := ABorder;
    AState := ACommand.CreateDefaultCommandUIState;
    AState.Enabled := True;
    ACommand.ForceExecute(AState);
  finally
    ACommand.Free;
  end;
end;

procedure TdxBorderShadingFormController.SetFillColor;
var
  I, J: Integer;
  ASelectedCell: TdxSelectedCellsIntervalInRow;
begin
  if FSelectedCells.IsSelectedEntireTable then
  begin
    FSelectedCells[0].Table.TableProperties.BackgroundColor := FillColor.Value;
    for J := 0 to FSelectedCells.RowsCount - 1 do
    begin
      ASelectedCell := FSelectedCells[J];
      for I := ASelectedCell.StartCellIndex to ASelectedCell.EndCellIndex do
        ASelectedCell.Row.Cells[I].Properties.ResetUse(TdxTableCellPropertiesOptions.MaskUseBackgroundColor);
    end;
  end
  else
  begin
    for J := 0 to FSelectedCells.RowsCount - 1 do
    begin
      ASelectedCell := FSelectedCells[J];
      for I := ASelectedCell.StartCellIndex to ASelectedCell.EndCellIndex do
        ASelectedCell.Row.Cells[I].BackgroundColor := FillColor.Value;
    end;
  end;
end;

end.
