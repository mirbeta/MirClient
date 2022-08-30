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

unit dxRichEdit.Dialogs.PageSetupController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Generics.Defaults, Generics.Collections, Types,
  dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.Utils.Properties,
  dxRichEdit.View.Core;

type
  { TdxPageSetupFormControllerParameters }

  TdxPageSetupFormControllerParameters = class(TdxFormControllerParameters)
  strict private
    FPageSetupInfo: TdxPageSetupInfo;
    FInitialTabPage: TdxPageSetupFormInitialTabPage;
  public
    constructor Create(const AControl: IdxRichEditControl; APageSetupInfo: TdxPageSetupInfo);

    property PageSetupInfo: TdxPageSetupInfo read FPageSetupInfo;
    property InitialTabPage: TdxPageSetupFormInitialTabPage read FInitialTabPage write FInitialTabPage;
  end;

  { TdxPageSetupFormController }

  TdxPageSetupFormController = class(TdxFormController)
  strict private
    FControl: IdxRichEditControl;
    FSourcePageSetupInfo: TdxPageSetupInfo;
    FPageSetupInfo: TdxPageSetupInfo;
    FValueUnitConverter: TdxDocumentModelUnitConverter;
    FCustomWidth: Integer;
    FCustomHeight: Integer;
    FInitialTabPage: TdxPageSetupFormInitialTabPage;
    function GetAvailableApplyType: TdxSectionPropertiesApplyTypes;
    function GetApplyType: TdxSectionPropertiesApplyType;
    procedure SetApplyType(const AValue: TdxSectionPropertiesApplyType);
    function GetLeftMargin: TdxNullableInteger;
    procedure SetLeftMargin(const AValue: TdxNullableInteger);
    function GetRightMargin: TdxNullableInteger;
    procedure SetRightMargin(const AValue: TdxNullableInteger);
    function GetTopMargin: TdxNullableInteger;
    procedure SetTopMargin(const AValue: TdxNullableInteger);
    function GetBottomMargin: TdxNullableInteger;
    procedure SetBottomMargin(const AValue: TdxNullableInteger);
    function GetPaperWidth: TdxNullableInteger;
    procedure SetPaperWidth(const AValue: TdxNullableInteger);
    function GetPaperHeight: TdxNullableInteger;
    procedure SetPaperHeight(const AValue: TdxNullableInteger);
    function GetDifferentFirstPage: TdxNullableBoolean;
    procedure SetDifferentFirstPage(const AValue: TdxNullableBoolean);
    function GetDifferentOddAndEvenPages: TdxNullableBoolean;
    procedure SetDifferentOddAndEvenPages(const AValue: TdxNullableBoolean);
    function GetSectionStartType: TdxNullableValue<TdxSectionStartType>;
    procedure SetSectionStartType(const AValue: TdxNullableValue<TdxSectionStartType>);
    function GetFullPaperKindList: TdxPaperKindList;
    function GetPaperKind: TdxNullableValue<TdxPaperKind>;
    procedure SetPaperKind(const AValue: TdxNullableValue<TdxPaperKind>);
    function GetLandscape: TdxNullableBoolean;
    procedure SetLandscape(const AValue: TdxNullableBoolean);
  protected
    procedure ChangeOrientation; virtual;
  public
    constructor Create(AControllerParameters: TdxPageSetupFormControllerParameters);
    destructor Destroy; override;
    function CalculateCustomSize: TSize;
    procedure UpdatePaperKind; virtual;
    procedure ApplyChanges; override;
    function IsTopBottomMarginsValid: Boolean;
    function IsLeftRightMarginsValid: Boolean;

    property Control: IdxRichEditControl read FControl;
    property SourcePageSetupInfo: TdxPageSetupInfo read FSourcePageSetupInfo;
    property ValueUnitConverter: TdxDocumentModelUnitConverter read FValueUnitConverter write FValueUnitConverter;

    property AvailableApplyType: TdxSectionPropertiesApplyTypes read GetAvailableApplyType;
    property ApplyType: TdxSectionPropertiesApplyType read GetApplyType write SetApplyType;
    property LeftMargin: TdxNullableInteger read GetLeftMargin write SetLeftMargin;
    property RightMargin: TdxNullableInteger read GetRightMargin write SetRightMargin;
    property TopMargin: TdxNullableInteger read GetTopMargin write SetTopMargin;
    property BottomMargin: TdxNullableInteger read GetBottomMargin write SetBottomMargin;
    property PaperWidth: TdxNullableInteger read GetPaperWidth write SetPaperWidth;
    property PaperHeight: TdxNullableInteger read GetPaperHeight write SetPaperHeight;
    property DifferentFirstPage: TdxNullableBoolean read GetDifferentFirstPage write SetDifferentFirstPage;
    property DifferentOddAndEvenPages: TdxNullableBoolean read GetDifferentOddAndEvenPages write SetDifferentOddAndEvenPages;
    property SectionStartType: TdxNullableValue<TdxSectionStartType> read GetSectionStartType write SetSectionStartType;
    property CustomWidth: Integer read FCustomWidth write FCustomWidth;
    property CustomHeight: Integer read FCustomHeight write FCustomHeight;
    property FullPaperKindList: TdxPaperKindList read GetFullPaperKindList;
    property PaperKind: TdxNullableValue<TdxPaperKind> read GetPaperKind write SetPaperKind;
    property Landscape: TdxNullableBoolean read GetLandscape write SetLandscape;

    property InitialTabPage: TdxPageSetupFormInitialTabPage read FInitialTabPage;
  end;

  { TdxPageSetupFormDefaults }

  TdxPageSetupFormDefaults = class abstract
  public const
    MinTopAndBottomMarginByDefault = -31680;
    MaxTopAndBottomMarginByDefault = 31680;
    MinLeftAndRightMarginByDefault = 0;
    MaxLeftAndRightMarginByDefault = 31680;
    MinPaperWidthAndHeightByDefault = 144;
    MaxPaperWidthAndHeightByDefault = 31680;
  end;

implementation

uses
  Math, dxCore, dxTypeHelpers, dxRichEdit.Commands.Dialogs;

{ TdxPageSetupFormControllerParameters }

constructor TdxPageSetupFormControllerParameters.Create(const AControl: IdxRichEditControl; APageSetupInfo: TdxPageSetupInfo);
begin
  inherited Create(AControl);
  Assert(APageSetupInfo <> nil, 'pageSetupInfo');
  FPageSetupInfo := APageSetupInfo;
end;

{ TdxPageSetupFormController }

constructor TdxPageSetupFormController.Create(AControllerParameters: TdxPageSetupFormControllerParameters);
var
  AUnitConverter: TdxDocumentModelUnitConverter;
begin
  inherited Create;
  FControl := AControllerParameters.Control;
  FSourcePageSetupInfo := AControllerParameters.PageSetupInfo;
  FPageSetupInfo := FSourcePageSetupInfo.Clone;
  AUnitConverter := FControl.InnerControl.DocumentModel.UnitConverter;
  if PaperWidth.HasValue then
    FCustomWidth := PaperWidth.Value
  else
    FCustomWidth := AUnitConverter.TwipsToModelUnits(TdxPaperSizeCalculator.CalculatePaperSize(TdxPaperKind.Letter).Width);
  if PaperHeight.HasValue then
    FCustomHeight := PaperHeight.Value
  else
    FCustomHeight := AUnitConverter.TwipsToModelUnits(TdxPaperSizeCalculator.CalculatePaperSize(TdxPaperKind.Letter).Height);
  FValueUnitConverter := AUnitConverter;
  FInitialTabPage := AControllerParameters.InitialTabPage;
end;

destructor TdxPageSetupFormController.Destroy;
begin
  FPageSetupInfo.Free;
  inherited Destroy;
end;

function TdxPageSetupFormController.GetAvailableApplyType: TdxSectionPropertiesApplyTypes;
begin
  Result := FPageSetupInfo.AvailableApplyType;
end;

function TdxPageSetupFormController.GetApplyType: TdxSectionPropertiesApplyType;
begin
  Result := FPageSetupInfo.ApplyType;
end;

procedure TdxPageSetupFormController.SetApplyType(const AValue: TdxSectionPropertiesApplyType);
begin
  FPageSetupInfo.ApplyType := AValue;
end;

function TdxPageSetupFormController.GetLeftMargin: TdxNullableInteger;
begin
  Result := FPageSetupInfo.LeftMargin;
end;

procedure TdxPageSetupFormController.SetLeftMargin(const AValue: TdxNullableInteger);
begin
  FPageSetupInfo.LeftMargin := AValue;
end;

function TdxPageSetupFormController.GetRightMargin: TdxNullableInteger;
begin
  Result := FPageSetupInfo.RightMargin;
end;

procedure TdxPageSetupFormController.SetRightMargin(const AValue: TdxNullableInteger);
begin
  FPageSetupInfo.RightMargin := AValue;
end;

function TdxPageSetupFormController.GetTopMargin: TdxNullableInteger;
begin
  Result := FPageSetupInfo.TopMargin;
end;

procedure TdxPageSetupFormController.SetTopMargin(const AValue: TdxNullableInteger);
begin
  FPageSetupInfo.TopMargin := AValue;
end;

function TdxPageSetupFormController.GetBottomMargin: TdxNullableInteger;
begin
  Result := FPageSetupInfo.BottomMargin;
end;

procedure TdxPageSetupFormController.SetBottomMargin(const AValue: TdxNullableInteger);
begin
  FPageSetupInfo.BottomMargin := AValue;
end;

function TdxPageSetupFormController.GetPaperWidth: TdxNullableInteger;
begin
  Result := FPageSetupInfo.PaperWidth;
end;

procedure TdxPageSetupFormController.SetPaperWidth(const AValue: TdxNullableInteger);
begin
  FPageSetupInfo.PaperWidth := AValue;
end;

function TdxPageSetupFormController.GetPaperHeight: TdxNullableInteger;
begin
  Result := FPageSetupInfo.PaperHeight;
end;

procedure TdxPageSetupFormController.SetPaperHeight(const AValue: TdxNullableInteger);
begin
  FPageSetupInfo.PaperHeight := AValue;
end;

function TdxPageSetupFormController.GetDifferentFirstPage: TdxNullableBoolean;
begin
  Result := FPageSetupInfo.DifferentFirstPage;
end;

procedure TdxPageSetupFormController.SetDifferentFirstPage(const AValue: TdxNullableBoolean);
begin
  FPageSetupInfo.DifferentFirstPage := AValue;
end;

function TdxPageSetupFormController.GetDifferentOddAndEvenPages: TdxNullableBoolean;
begin
  Result := FPageSetupInfo.DifferentOddAndEvenPages;
end;

procedure TdxPageSetupFormController.SetDifferentOddAndEvenPages(const AValue: TdxNullableBoolean);
begin
  FPageSetupInfo.DifferentOddAndEvenPages := AValue;
end;

function TdxPageSetupFormController.GetSectionStartType: TdxNullableValue<TdxSectionStartType>;
begin
  Result := FPageSetupInfo.SectionStartType;
end;

procedure TdxPageSetupFormController.SetSectionStartType(const AValue: TdxNullableValue<TdxSectionStartType>);
begin
  FPageSetupInfo.SectionStartType := AValue;
end;

function TdxPageSetupFormController.GetFullPaperKindList: TdxPaperKindList;
begin
  Result := TdxChangeSectionPaperKindCommand.FullPaperKindList;
end;

function TdxPageSetupFormController.GetPaperKind: TdxNullableValue<TdxPaperKind>;
begin
  Result := FPageSetupInfo.PaperKind;
end;

procedure TdxPageSetupFormController.SetPaperKind(const AValue: TdxNullableValue<TdxPaperKind>);
var
  ASize: TSize;
  AUnitConverter: TdxDocumentModelUnitConverter;
  ATemp: Integer;
begin
  if PaperKind = AValue then
    Exit;
  FPageSetupInfo.PaperKind := AValue;
  if PaperKind.HasValue and (PaperKind.Value <> TdxPaperKind.Custom) then
  begin
    AUnitConverter := FControl.InnerControl.DocumentModel.UnitConverter;
    ASize := AUnitConverter.TwipsToModelUnits(TdxPaperSizeCalculator.CalculatePaperSize(PaperKind.Value));
    if Landscape.HasValue and Landscape.Value then
    begin
      ATemp := ASize.Width;
      ASize.Width := ASize.Height;
      ASize.Height := ATemp;
    end;
  end
  else
    ASize := CalculateCustomSize;

  PaperWidth := ASize.Width;
  PaperHeight := ASize.Height;
end;

function TdxPageSetupFormController.GetLandscape: TdxNullableBoolean;
begin
  Result := FPageSetupInfo.Landscape;
end;

procedure TdxPageSetupFormController.SetLandscape(const AValue: TdxNullableBoolean);
begin
  if Landscape = AValue then
    Exit;
  if Landscape.HasValue then
  begin
    FPageSetupInfo.Landscape := AValue;
    ChangeOrientation;
  end
  else
    FPageSetupInfo.Landscape := AValue;
end;

function TdxPageSetupFormController.CalculateCustomSize: TSize;
begin
  if Landscape.HasValue and Landscape.Value then
    Exit(TSize.Create(CustomHeight, CustomWidth))
  else
    Exit(TSize.Create(CustomWidth, CustomHeight));
end;

procedure TdxPageSetupFormController.ChangeOrientation;
var
  ACustomValue: Integer;
  AValue, ALeft, ARight, ATop, ABottom: TdxNullableInteger;
begin
  if not Landscape.HasValue then
    Exit;

  ACustomValue := CustomWidth;
  CustomWidth := CustomHeight;
  CustomHeight := ACustomValue;

  AValue := PaperWidth;
  PaperWidth := PaperHeight;
  PaperHeight := AValue;

  ALeft := LeftMargin;
  ARight := RightMargin;
  ATop := Abs(TopMargin);
  ABottom := Abs(BottomMargin);

  if Landscape.Value then
  begin
    LeftMargin := ATop;
    RightMargin := ABottom;
    TopMargin := ARight;
    BottomMargin := ALeft;
  end
  else
  begin
    LeftMargin := ABottom;
    RightMargin := ATop;
    TopMargin := ALeft;
    BottomMargin := ARight;
  end;
end;

procedure TdxPageSetupFormController.UpdatePaperKind;
var
  AUnitConverter: TdxDocumentModelUnitConverter;
  APaperSize, ASize: TSize;
begin
  if not PaperWidth.HasValue then
    Exit;
  if not PaperHeight.HasValue then
    Exit;

  AUnitConverter := FControl.DocumentModel.UnitConverter;
  if Landscape.HasValue and Landscape.Value then
    APaperSize := TSize.Create(PaperHeight.Value, PaperWidth.Value)
  else
    APaperSize := TSize.Create(PaperWidth.Value, PaperHeight.Value);

  ASize := AUnitConverter.ModelUnitsToTwips(APaperSize);
  FPageSetupInfo.PaperKind := TdxPaperSizeCalculator.CalculatePaperKind(ASize, TdxPaperKind.Custom);
end;

procedure TdxPageSetupFormController.ApplyChanges;
begin
  FSourcePageSetupInfo.CopyFrom(FPageSetupInfo);
end;

function TdxPageSetupFormController.IsTopBottomMarginsValid: Boolean;
var
  ATopMargin, ABottomMargin, APaperHeight: Integer;
begin
  if TopMargin.HasValue then
    ATopMargin := Abs(TopMargin.Value)
  else
    ATopMargin := 0;
  if BottomMargin.HasValue then
    ABottomMargin := Abs(BottomMargin.Value)
  else
    ABottomMargin := 0;
  if PaperHeight.HasValue  then
    APaperHeight := PaperHeight.Value
  else
    APaperHeight := 0;
  Result := (ATopMargin + ABottomMargin) < APaperHeight;
end;

function TdxPageSetupFormController.IsLeftRightMarginsValid: Boolean;
var
  ALeftMargin, ARightMargin, APaperWidth: Integer;
begin
  if LeftMargin.HasValue then
    ALeftMargin := LeftMargin.Value
  else
    ALeftMargin := 0;
  if RightMargin.HasValue then
    ARightMargin := RightMargin.Value
  else
    ARightMargin := 0;
  if PaperWidth.HasValue then
    APaperWidth := PaperWidth.Value
  else
    APaperWidth := 0;
  Result := (ALeftMargin + ARightMargin) < APaperWidth;
end;

end.
