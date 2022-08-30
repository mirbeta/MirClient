{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit cxDataControllerConditionalFormatting;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Graphics, Classes, Generics.Defaults, Generics.Collections, Forms,
  dxCore, dxCoreClasses, cxClasses, cxControls, cxLookAndFeels, cxGeometry, cxVariants,
  cxCustomData, cxGraphics, cxInplaceContainer, cxStorage, cxEdit,
  dxSpreadSheetTypes,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetCoreFormulasParser,
  dxSpreadSheetCoreStyles,
  dxSpreadSheetClasses,
  dxSpreadSheetStyles,
  dxSpreadSheetConditionalFormatting;

type
  TcxDataControllerConditionalFormattingProvider = class;
  TcxDataControllerConditionalFormatting = class;

  { TcxDataControllerConditionalFormattingCellData }

  TcxDataControllerConditionalFormattingCellData = class(TInterfacedObject,
    IdxSpreadSheetCellData)
  strict private
    FProvider: TcxDataControllerConditionalFormattingProvider;
    FItemIndex: Integer;
    FRecordIndex: Integer;
  protected
    // IdxSpreadSheetCellData
    function GetAsError: TdxSpreadSheetFormulaErrorCode;
    function GetAsFloat: Double;
    function GetAsFormula: TObject{TdxSpreadSheetCustomFormula};
    function GetAsVariant: Variant;
    function GetDataType: TdxSpreadSheetCellDataType;
    function GetIsEmpty: Boolean;
    function IsNumericValue: Boolean;
  public
    constructor Create(AProvider: TcxDataControllerConditionalFormattingProvider; ARecordIndex, AItemIndex: Integer);
  end;

  { TcxDataControllerConditionalFormattingFormulaFieldReference }

  TcxDataControllerConditionalFormattingFormulaFieldReference = class(TdxSpreadSheetFormulaAreaReference)
  strict private
    FOwner: TcxDataControllerConditionalFormattingProvider;
  protected
    function ReferenceToString: string; overload; override;
    function ReferenceToString(const ARow, AColumn: TdxSpreadSheetReference): string; overload; override;

    procedure FieldIndexChanged(AOldIndex, ANewIndex: Integer);
  public
    constructor Create(AOwner: TcxDataControllerConditionalFormattingProvider; AColumn: Integer); reintroduce;
  end;

  { TcxDataControllerConditionalFormattingFormulaParser }

  TcxDataControllerConditionalFormattingFormulaParser = class(TdxSpreadSheetCustomFormulaParser)
  strict private const
  {$REGION 'strict private const'}
    FieldStartMark = '[';
    FieldFinishMark = ']';
  {$ENDREGION 'strict private const'}
  strict private
    FOwner: TcxDataControllerConditionalFormattingProvider;

    function FindFieldByName(const AName: string): Integer;
    function IsFieldName(var APosition: Integer; ALength: Integer; out AToken: TdxSpreadSheetFormulaToken): Boolean; inline;
  protected
    procedure RegisterTokenControllers; override;
  public
    constructor Create(AOwner: TcxDataControllerConditionalFormattingProvider); reintroduce;
  end;

  { TcxDataControllerConditionalFormattingFormatSettings }

  TcxDataControllerConditionalFormattingFormatSettings = class(TdxSpreadSheetFormatSettings)
  strict private
    FNativeReferences: Boolean;
  protected
    function GetLocaleID: Integer; override;
  public
    procedure Assign(ASource: TdxSpreadSheetCustomFormatSettings); override;
    //
    property NativeReferences: Boolean read FNativeReferences write FNativeReferences;
  end;

  { TcxDataControllerConditionalFormattingInvariantFormatSettings }

  TcxDataControllerConditionalFormattingInvariantFormatSettings = class(TcxDataControllerConditionalFormattingFormatSettings)
  protected
    function GetLocaleID: Integer; override;
  end;

  { TcxDataControllerConditionalFormattingFormulaController }

  TcxDataControllerConditionalFormattingFormulaController = class(TdxSpreadSheetCustomFormulaController)
  strict private
    FOwner: TcxDataControllerConditionalFormattingProvider;
  protected
    function CreateParser: TObject; {TdxSpreadSheetCustomFormulaParser} override;
    function GetFormatSettings: TdxSpreadSheetFormatSettings; override;
  public
    constructor Create(AOwner: TcxDataControllerConditionalFormattingProvider);
  end;

  { TcxDataControllerConditionalFormattingStyleViewInfo }

  TcxDataControllerConditionalFormattingStyleViewInfo = class(TdxSpreadSheetConditionalFormattingStyleViewInfo)
  strict private
    FCalculatedBounds: TRect;
  protected
    procedure ValidateBounds(const R: TRect);
  public
    procedure AfterDrawCellBackground(ACanvas: TcxCanvas; ABackgroundColor: TColor);
    procedure AfterDrawCellValue(ACanvas: TcxCanvas);
    procedure Calculate(const ABounds, AContentBounds: TRect); overload; override;
  end;

  { IcxDataControllerConditionalFormattingProviderOwner }

  IcxDataControllerConditionalFormattingProviderOwner = interface
  ['{52592036-5D36-435E-ABBF-AFFCEE73F577}']
    function GetConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider;
  end;

  { TcxDataControllerConditionalFormattingProvider }

  TcxDataControllerConditionalFormattingProvider = class abstract(TcxIUnknownObject,
    IdxSpreadSheetViewData,
    IdxSpreadSheetCellStyleOwner,
    IdxSpreadSheetConditionalFormattingOwner,
    IdxSpreadSheetConditionalFormatting,
    IcxStoredParent,
    IcxStoredParent2,
    IcxStoredObject,
    IcxStoredObject2,
    IdxDialogOwner)
  public const
    DefaultArea: TRect = (Left: 0; Top: 0; Right: 0; Bottom: MaxInt);
  strict private type
  {$REGION 'strict private type'}
    TCellStyleCacheKey = TRect;

    TCellStyleCache = class(TObjectDictionary<TCellStyleCacheKey, TdxSpreadSheetCellDisplayStyle>);

    TStyleCache = class(TObjectDictionary<TPoint, TCellStyleCache>)
    public
      procedure AddStyle(const ACell: TPoint; const AKey: TCellStyleCacheKey; AStyle: TdxSpreadSheetCellDisplayStyle);
      function TryGetStyle(const ACell: TPoint; out AStyle: TdxSpreadSheetCellDisplayStyle): Boolean;
      function TryGetCellStyle(const ACell: TPoint; const AKey: TCellStyleCacheKey; out AStyle: TdxSpreadSheetCellDisplayStyle): Boolean;
    end;
    TStyleViewInfoCache = class(TObjectDictionary<TPoint, TcxDataControllerConditionalFormattingStyleViewInfo>);
  {$ENDREGION}
  strict private class var
    FFormatSettingsInvariant: TcxDataControllerConditionalFormattingInvariantFormatSettings;
  protected
    class procedure Initialize; static;
    class procedure Finalize; static;
  strict private
    FCustomSelectedArea: TRect;
    FDataController: TcxCustomDataController;
    FFieldsOrderLockCount: Integer;
    FIsDestroying: Boolean;
    FLockCount: Integer;
    FLoadingStream: TMemoryStream;
    FStyleCache: TStyleCache;
    FStyleViewInfoCache: TStyleViewInfoCache;

    function IsOwnerLocked: Boolean;
    procedure DataControllerChangedHandler(Sender: TObject);
    procedure FieldIndexChangedHandler(Sender: TObject; AOldIndex, ANewIndex: Integer);
  strict protected
    FCellStyles: TdxSpreadSheetCellStyles;
    FConditionalFormatting: TcxDataControllerConditionalFormatting;
    FFormatSettings: TcxDataControllerConditionalFormattingFormatSettings;
    FFormulaController: TcxDataControllerConditionalFormattingFormulaController;
  protected
    function CalculateBestFitWidth(const ACell: TPoint; ABaseWidth: Integer): Integer;
    function CreateCellStyles: TdxSpreadSheetCellStyles; virtual;
    function CreateConditionalFormatting: TcxDataControllerConditionalFormatting; virtual;
    function CreateFormatSettings: TcxDataControllerConditionalFormattingFormatSettings; virtual;
    function CreateFormulaController: TcxDataControllerConditionalFormattingFormulaController; virtual;
    procedure ForEachRow(ARow, AStart, AFinish: Integer; AProc: TdxSpreadSheetViewForEachCellProc; AGoForward: Boolean);

    procedure ClearCache;
    procedure ClearCacheCore; virtual;
    procedure ConditionalFormattingChanged; virtual;
    function TryGetStyle(const ACell: TPoint; out AStyle: TdxSpreadSheetCellDisplayStyle): Boolean;

    procedure Changed; virtual;
    procedure DoBeginUpdate; virtual; abstract;
    procedure DoEndUpdate; virtual; abstract;
    function DoGetCellValue(ARecordIndex, AItemIndex: Integer): Variant; virtual;
    function DoGetParentForm: TCustomForm; virtual; abstract;
    function EditCellViewInfoToPoint(ACell: TObject): TPoint; virtual; abstract;
    function FindFieldByItem(AItem: TObject): Integer;
    function FindFieldByName(const AName: string): Integer;
    function GetCellBounds(ACellViewInfo: TObject): TRect; virtual; abstract;
    function GetCellValue(ARecordIndex, AItemIndex: Integer): Variant;
    function GetConditionalFormattingStyleViewInfo(ACell: TObject): TcxDataControllerConditionalFormattingStyleViewInfo;
    function GetDisplayName(AField: TcxCustomDataField): string; virtual; abstract;
    function GetEditProperties(AItem: TObject): TcxCustomEditProperties; virtual; abstract;
    function GetFocusedItem: TObject; virtual; abstract;
    function GetRecordCount: Integer; virtual;
    function GetScaleFactor: TdxScaleFactor; virtual; abstract;
    function IsDesigning: Boolean;
    function IsFieldVisible(AField: TcxCustomDataField): Boolean; virtual; abstract;
    function IsUpdateLocked: Boolean;
    function ReferenceToString(AColumn: Integer): string; overload;
    function ReferenceToString(const AArea: TRect): string; overload;

    function IsStoringSupported: Boolean;
    procedure DefineBinaryProperty(AFiler: TFiler);
    procedure LoadFromStream(AStream: TStream);
    procedure Loaded;
    procedure Read(AStream: TStream); virtual;
    procedure Write(AStream: TStream); virtual;

    procedure BeginUpdateFieldsOrder;
    procedure EndUpdateFieldsOrder;

    procedure AfterDrawCellBackground(ACellViewInfo: TObject; ABackgroundColor: TColor; ACanvas: TcxCanvas);
    procedure AfterDrawCellValue(ACellViewInfo: TObject; ACanvas: TcxCanvas);
    function CalculateStyle(const AStyle: TdxSpreadSheetCellDisplayStyle; ARow, AColumn: Integer; ACell: IdxSpreadSheetCellData): Boolean; virtual;
    procedure CalculateCellEditorBounds(ACellViewInfo: TObject; var R: TRect);
    procedure CalculateStyleViewInfo(ACellViewInfo: TObject);
    procedure CalculateViewParams(var AParams: TcxViewParams; ARecordIndex, AItemIndex: Integer); virtual;
    procedure CanDrawCellValue(ACellViewInfo: TObject; var Allow: Boolean);
    function FormatDisplayValue(const ACellViewInfo: TObject; const AValue: Variant): Variant;

    class function CalculateNewIndex(ACheckIndex, AOldIndex, ANewIndex: Integer): Integer; static;

  {$REGION 'Interfaces'}
    // IdxSpreadSheetViewData
    procedure ForEachCell(const AArea: TRect; AProc: TdxSpreadSheetViewForEachCellProc; AGoForward: Boolean = True);
    function GetCellData(const ARow, AColumn: Integer): IdxSpreadSheetCellData; virtual;
    function GetMaxColumnIndex: Integer;
    function GetMaxRowIndex: Integer;
    function GetNextColumnWithNonEmptyCell(const ARow, AColumn: Integer; const AGoForward: Boolean = True): Integer;
    function GetNextRowWithNonEmptyCell(const ARow, AColumn: Integer; const AGoForward: Boolean = True): Integer;
    function IsRowVisible(const ARow: Integer): Boolean; virtual; abstract;
    procedure SetCellData(const ARow, AColumn: Integer; const AValue: Variant; const AErrorCode: TdxSpreadSheetFormulaErrorCode);

    // IdxSpreadSheetCellStyleOwner
    function GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
    procedure CellStyleChanged;
    procedure CellStyleChanging;
    function GetCellStyles: TdxSpreadSheetCellStyles;
    procedure ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);

    // IdxSpreadSheetConditionalFormattingOwner
    function GetFormulaController: TdxSpreadSheetCustomFormulaController;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetSelectionArea: TRect; virtual;
    function IsRightToLeft: Boolean; virtual; abstract;

    // IdxSpreadSheetConditionalFormatting
    function GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;

    // IdxDialogOwner
    function GetLookAndFeel: TcxLookAndFeel; virtual; abstract;
    function GetOwner: TComponent; virtual; abstract;
    function GetParentForm: TCustomForm;

    // IcxStoredParent
    function CreateChild(const AObjectName, AClassName: string): TObject;
    procedure DeleteChild(const AObjectName: string; AObject: TObject);
    procedure GetChildren(AChildren: TStringList);

    // IcxStoredParent2
    procedure StoringBegin;
    procedure StoringEnd;

    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean;
    procedure GetPropertyValue(const AName: string; var AValue: Variant);
    procedure SetPropertyValue(const AName: string; const AValue: Variant);
    // IcxStoredObject2
    function CanDelete: Boolean;
  {$ENDREGION 'Interfaces'}

    function IsActive: Boolean;
    function IsDetailMode: Boolean; virtual;
    procedure Recalculate;

    property CellStyles: TdxSpreadSheetCellStyles read GetCellStyles;
    property CustomSelectedArea: TRect read FCustomSelectedArea write FCustomSelectedArea;
    property DataController: TcxCustomDataController read FDataController;
    property FormatSettings: TcxDataControllerConditionalFormattingFormatSettings read FFormatSettings;
    property FormulaController: TcxDataControllerConditionalFormattingFormulaController read FFormulaController;
    property IsDestroying: Boolean read FIsDestroying;
    property StyleCache: TStyleCache read FStyleCache;
    property StyleViewInfoCache: TStyleViewInfoCache read FStyleViewInfoCache;
  public
    constructor Create(ADataController: TcxCustomDataController);
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    property ConditionalFormatting: TcxDataControllerConditionalFormatting read FConditionalFormatting;
  end;

  { TcxDataControllerConditionalFormatting }

  TcxDataControllerConditionalFormatting = class(TdxSpreadSheetCustomConditionalFormatting)
  strict private
    FProvider: TcxDataControllerConditionalFormattingProvider;
    FExcludeFilteredOutRecords: Boolean;
    function GetDataController: TcxCustomDataController;
    procedure SetExcludeFilteredOutRecords(const Value: Boolean);
  protected
    function CanValidateExpressionRuleResultValue: Boolean; override;
    procedure DoChanged; override;
    function IsStyleBorderSupported: Boolean; override;
    function IsValueFormattingSupported: Boolean; override;
    function GetFormulaEditMask: string; override;
    procedure SetCustomAreaInfoCache(const Value: TObjectDictionary<string, TdxSpreadSheetConditionalFormattingAreaInfo>);
    procedure SetCustomProvider(const Value: TcxDataControllerConditionalFormattingProvider);

    property Provider: TcxDataControllerConditionalFormattingProvider read FProvider;
  public
    constructor Create(AOwner: TcxDataControllerConditionalFormattingProvider); reintroduce;

    procedure Add(const AField: TcxCustomDataField; ARuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass; out ARule); overload;
    procedure Add(const AFieldIndex: Integer; ARuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass; out ARule); overload;
    procedure Add(const AFieldName: string; ARuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass; out ARule); overload;

    function GetFieldDisplayName(AField: TcxCustomDataField): string;
    function IsFieldVisible(AField: TcxCustomDataField): Boolean;
    function ReferencesToString(const AAreas: TdxSpreadSheetAreaList): string; override;
    function CanShowRulesManagerDialog: Boolean;
    procedure ShowRulesManagerDialog;

    property ExcludeFilteredOutRecords: Boolean read FExcludeFilteredOutRecords write SetExcludeFilteredOutRecords;

    property DataController: TcxCustomDataController read GetDataController;
  end;

  { TcxCustomControlControllerConditionalFormattingProvider }

  TcxCustomControlControllerConditionalFormattingProvider = class abstract(TcxDataControllerConditionalFormattingProvider)
  protected
    function EditCellViewInfoToPoint(ACellViewInfo: TObject): TPoint; override;

    function GetCellBounds(ACellViewInfo: TObject): TRect; override;
    function GetController: TcxCustomControlController; virtual; abstract;
    function GetDisplayName(AField: TcxCustomDataField): string; override;
    function GetItemDisplayName(AItem: TcxCustomInplaceEditContainer): string; virtual; abstract;
    function GetEditProperties(AItem: TObject): TcxCustomEditProperties; override;
    function GetFocusedItem: TObject; override;
    function IsFieldVisible(AField: TcxCustomDataField): Boolean; override;
    function IsItemVisible(AItem: TcxCustomInplaceEditContainer): Boolean; virtual; abstract;

    property Controller: TcxCustomControlController read GetController;
  end;

  { TcxDataControllerConditionalFormattingRulesManagerDialogProvider }

  TcxDataControllerConditionalFormattingRulesManagerDialogProvider = class sealed
  public type
  {$REGION 'public type'}
    TAdapter = class abstract(TPersistent)
    public
      procedure Execute(AProvider: TcxDataControllerConditionalFormattingProvider); virtual; abstract;
    end;
    TAdapterClass = class of TAdapter;
  {$ENDREGION}
  strict private class var
  {$REGION 'strict private class var'}
    FRulesManagerDialogs: TList<TAdapterClass>;
  {$ENDREGION}
  strict private
    class function GetRulesManagerDialogs: TList<TAdapterClass>; static;
{$IFDEF DELPHIXE}
    class destructor Finalize;
{$ELSE}
  private
    class procedure Finalize; static;
{$ENDIF}
  protected
    class property RulesManagerDialogs: TList<TAdapterClass> read GetRulesManagerDialogs;
  public
    class procedure RequiresRulesManagerDialogUnits(AClass: TClass; const AProc: TGetStrProc);  overload; static;
    class procedure RequiresRulesManagerDialogUnits(const ARootComponent: TComponent; const AProc: TGetStrProc); overload; static;
    class procedure RegisterRulesManagerDialog(AAdapter: TAdapterClass); static;
    class procedure UnregisterRulesManagerDialog(AAdapter: TAdapterClass); static;

    class function CanShowRulesManagerDialog: Boolean; static;
    class procedure ShowRulesManagerDialog(AProvider: TcxDataControllerConditionalFormattingProvider); static;
  end;

implementation

uses
  SysUtils, RTLConsts, Math, Variants,
  dxTypeHelpers, cxDrawTextUtils, dxGDIPlusClasses, dxGDIPlusAPI, cxTextEdit,
  dxSpreadSheetGraphics,
  dxSpreadSheetNumberFormat,
  dxSpreadSheetConditionalFormattingRules, dxSpreadSheetUtils, dxSpreadSheetCoreStrs, dxStringHelper;

type
  TdxSpreadSheetCellFontAccess = class(TdxSpreadSheetCellFont);
  TcxCustomDataControllerAccess = class(TcxCustomDataController);
  TcxEditCellViewInfoAccess = class(TcxEditCellViewInfo);
  TcxCustomInplaceEditContainerAccess = class(TcxCustomInplaceEditContainer);
  TcxCustomTextEditPropertiesAccess = class(TcxCustomTextEditProperties);
  TdxSpreadSheetConditionalFormattingRuleExpressionAccess = class(TdxSpreadSheetConditionalFormattingRuleExpression);

{ TcxDataControllerConditionalFormattingCellData }

constructor TcxDataControllerConditionalFormattingCellData.Create(
  AProvider: TcxDataControllerConditionalFormattingProvider; ARecordIndex, AItemIndex: Integer);
begin
  inherited Create;
  FProvider := AProvider;
  FRecordIndex := ARecordIndex;
  FItemIndex := AItemIndex;
end;

function TcxDataControllerConditionalFormattingCellData.GetAsError: TdxSpreadSheetFormulaErrorCode;
begin
  Result := ecNone;
end;

function TcxDataControllerConditionalFormattingCellData.GetAsFloat: Double;
begin
  Result := GetAsVariant;
end;

function TcxDataControllerConditionalFormattingCellData.GetAsFormula: TObject{TdxSpreadSheetCustomFormula};
begin
  Result := nil;
end;

function TcxDataControllerConditionalFormattingCellData.GetAsVariant: Variant;
begin
  Result := FProvider.GetCellValue(FRecordIndex, FItemIndex);
end;

function TcxDataControllerConditionalFormattingCellData.GetDataType: TdxSpreadSheetCellDataType;
begin
  if GetIsEmpty then
    Result := cdtBlank
  else
    Result := dxGetDataTypeByVariantValue(GetAsVariant);
end;

function TcxDataControllerConditionalFormattingCellData.GetIsEmpty: Boolean;
var
  AValue: Variant;
begin
  AValue := GetAsVariant;
  Result := VarIsEmpty(AValue) or VarIsNull(AValue);
end;

function TcxDataControllerConditionalFormattingCellData.IsNumericValue: Boolean;
var
  AFloatValue: Single;
  AVariant: Variant;
begin
  AVariant := GetAsVariant;
  Result := VarIsNumericEx(AVariant) or VarIsDate(AVariant) or
    VarIsStr(AVariant) and TryStrToFloat(VarToStr(AVariant), AFloatValue);
end;

{ TcxDataControllerConditionalFormattingFormulaFieldReference }

constructor TcxDataControllerConditionalFormattingFormulaFieldReference.Create(
  AOwner: TcxDataControllerConditionalFormattingProvider; AColumn: Integer);
begin
  inherited Create(0, AColumn, MaxInt, AColumn, True, True, True, True);
  FOwner := AOwner;
end;

function TcxDataControllerConditionalFormattingFormulaFieldReference.ReferenceToString: string;
begin
  Result := ReferenceToString(FRow, FColumn);
end;

function TcxDataControllerConditionalFormattingFormulaFieldReference.ReferenceToString(
  const ARow, AColumn: TdxSpreadSheetReference): string;
begin
  Result := FOwner.ReferenceToString(AColumn.Offset);
end;

procedure TcxDataControllerConditionalFormattingFormulaFieldReference.FieldIndexChanged(AOldIndex, ANewIndex: Integer);
begin
  FColumn.Offset := TcxDataControllerConditionalFormattingProvider.CalculateNewIndex(FColumn.Offset, AOldIndex, ANewIndex);
  FColumn2.Offset := TcxDataControllerConditionalFormattingProvider.CalculateNewIndex(FColumn2.Offset, AOldIndex, ANewIndex);
end;

{ TcxDataControllerConditionalFormattingFormulaParser }

constructor TcxDataControllerConditionalFormattingFormulaParser.Create(AOwner: TcxDataControllerConditionalFormattingProvider);
begin
  inherited Create;
  FOwner := AOwner;
  FormatSettings.Assign(AOwner.FormatSettings);
end;

procedure TcxDataControllerConditionalFormattingFormulaParser.RegisterTokenControllers;
begin
  AddTokenController(IsFieldName);
  inherited RegisterTokenControllers;
end;

function TcxDataControllerConditionalFormattingFormulaParser.FindFieldByName(const AName: string): Integer;
begin
  Result := FOwner.FindFieldByName(AName);
end;

function TcxDataControllerConditionalFormattingFormulaParser.IsFieldName(
  var APosition: Integer; ALength: Integer; out AToken: TdxSpreadSheetFormulaToken): Boolean;
var
  AItemIndex: Integer;
  AName: string;
  AStart, AFinish: Integer;
  L: Integer;
begin
  Result := CheckText(APosition, FieldStartMark);
  if Result then
  begin
    AFinish := APosition + ALength;
    Inc(APosition);
    AStart := APosition;
    while (APosition < AFinish) and not CheckText(APosition, FieldFinishMark) do
      Inc(APosition);
    if APosition > AFinish then
    begin
      SetErrorIndex(APosition - 1);
      Exit(False);
    end;
    L := APosition - AStart;
    AName := Copy(FFormulaText, AStart, L);
    AItemIndex := FindFieldByName(AName);
    Inc(APosition);
    if AItemIndex = -1 then
    begin
      SetErrorIndex(APosition - 1);
      Exit(False);
    end;
    AToken := TcxDataControllerConditionalFormattingFormulaFieldReference.Create(FOwner, AItemIndex);
  end;
end;

{ TcxDataControllerConditionalFormattingFormatSettings }

procedure TcxDataControllerConditionalFormattingFormatSettings.Assign(ASource: TdxSpreadSheetCustomFormatSettings);
begin
  inherited;
  if ASource is TcxDataControllerConditionalFormattingFormatSettings then
    NativeReferences := TcxDataControllerConditionalFormattingFormatSettings(ASource).NativeReferences;
end;

function TcxDataControllerConditionalFormattingFormatSettings.GetLocaleID: Integer;
begin
  Result := GetThreadLocale;
end;

{ TcxDataControllerConditionalFormattingInvariantFormatSettings }

function TcxDataControllerConditionalFormattingInvariantFormatSettings.GetLocaleID: Integer;
begin
  Result := dxGetInvariantLocaleID;
end;

{ TcxDataControllerConditionalFormattingFormulaController }

constructor TcxDataControllerConditionalFormattingFormulaController.Create(AOwner: TcxDataControllerConditionalFormattingProvider);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TcxDataControllerConditionalFormattingFormulaController.CreateParser: TObject; {TdxSpreadSheetCustomFormulaParser}
begin
  Result := TcxDataControllerConditionalFormattingFormulaParser.Create(FOwner);
end;

function TcxDataControllerConditionalFormattingFormulaController.GetFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := FOwner.FormatSettings;
end;

{ TcxDataControllerConditionalFormattingStyleViewInfo }

procedure TcxDataControllerConditionalFormattingStyleViewInfo.AfterDrawCellBackground(ACanvas: TcxCanvas; ABackgroundColor: TColor);
var
  ABrush: TdxSpreadSheetCellBrush;
begin
  ABrush := Style.Brush;
  if (ABrush.ForegroundColor <> clNone) and (ABrush.Style <> sscfsSolid) then
    dxSpreadSheetDrawBackground(ACanvas, FCalculatedBounds, ABackgroundColor, ABrush.ForegroundColor, ABrush.Style);
  if not Style.DataBar.IsEmpty then
    ACanvas.Brush.Color := ABackgroundColor;
  Draw(ACanvas, dsFirst);
end;

procedure TcxDataControllerConditionalFormattingStyleViewInfo.AfterDrawCellValue(ACanvas: TcxCanvas);
begin
  Draw(ACanvas, dsSecond);
end;

procedure TcxDataControllerConditionalFormattingStyleViewInfo.Calculate(const ABounds, AContentBounds: TRect);
begin
  FCalculatedBounds := AContentBounds;
  inherited Calculate(ABounds, AContentBounds);
end;

procedure TcxDataControllerConditionalFormattingStyleViewInfo.ValidateBounds(const R: TRect);
begin
  if not R.IsEqual(FCalculatedBounds) then
    Calculate(R, R);
end;

{ TcxDataControllerConditionalFormattingProvider.TStyleCache }

procedure TcxDataControllerConditionalFormattingProvider.TStyleCache.AddStyle(const ACell: TPoint; const AKey: TCellStyleCacheKey; AStyle: TdxSpreadSheetCellDisplayStyle);
var
  ACache: TCellStyleCache;
begin
  if not TryGetValue(ACell, ACache) then
  begin
    ACache := TCellStyleCache.Create([doOwnsValues]);
    Add(ACell, ACache);
  end;
  ACache.AddOrSetValue(AKey, AStyle);
end;

function TcxDataControllerConditionalFormattingProvider.TStyleCache.TryGetStyle(const ACell: TPoint; out AStyle: TdxSpreadSheetCellDisplayStyle): Boolean;
var
  AKey: TCellStyleCacheKey;
  ACache: TCellStyleCache;
begin
  AStyle := nil;
  Result := TryGetValue(ACell, ACache);
  if Result and (ACache.Count > 0) then
  begin
    for AKey in ACache.Keys do
    begin
      AStyle := ACache[AKey];
      Break;
    end;
    Result := AStyle <> nil;
  end;
end;

function TcxDataControllerConditionalFormattingProvider.TStyleCache.TryGetCellStyle(const ACell: TPoint; const AKey: TCellStyleCacheKey; out AStyle: TdxSpreadSheetCellDisplayStyle): Boolean;
var
  ACache: TCellStyleCache;
begin
  AStyle := nil;
  Result := TryGetValue(ACell, ACache);
  if Result then
    Result := ACache.TryGetValue(AKey, AStyle);
end;

{ TcxDataControllerConditionalFormattingProvider }

constructor TcxDataControllerConditionalFormattingProvider.Create(ADataController: TcxCustomDataController);
begin
  inherited Create;
  FDataController := ADataController;
  FDataController.AddDataChangedListener(Self, DataControllerChangedHandler);
  FDataController.OnFieldIndexChanged.Add(FieldIndexChangedHandler);
  FStyleCache := TStyleCache.Create([doOwnsValues]);
  FStyleViewInfoCache := TStyleViewInfoCache.Create([doOwnsValues]);
  if not IsDetailMode then
  begin
    FCellStyles := CreateCellStyles;
    FFormulaController := CreateFormulaController;
    FConditionalFormatting := CreateConditionalFormatting;
    FFormatSettings := CreateFormatSettings;
  end;
end;

destructor TcxDataControllerConditionalFormattingProvider.Destroy;
begin
  FDataController.RemoveDataChangedListener(Self, DataControllerChangedHandler);
  FDataController.OnFieldIndexChanged.Remove(FieldIndexChangedHandler);
  FreeAndNil(FStyleViewInfoCache);
  FreeAndNil(FStyleCache);
  FreeAndNil(FLoadingStream);
  if not IsDetailMode then
  begin
    FreeAndNil(FFormatSettings);
    FreeAndNil(FConditionalFormatting);
    FreeAndNil(FFormulaController);
    FreeAndNil(FCellStyles);
  end;
  inherited Destroy;
end;

class procedure TcxDataControllerConditionalFormattingProvider.Initialize;
begin
  FFormatSettingsInvariant := TcxDataControllerConditionalFormattingInvariantFormatSettings.Create;
end;

class procedure TcxDataControllerConditionalFormattingProvider.Finalize;
begin
  FreeAndNil(FFormatSettingsInvariant);
end;

procedure TcxDataControllerConditionalFormattingProvider.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FIsDestroying := True;
end;

procedure TcxDataControllerConditionalFormattingProvider.AfterDrawCellBackground(ACellViewInfo: TObject; ABackgroundColor: TColor; ACanvas: TcxCanvas);
var
  AViewInfo: TcxDataControllerConditionalFormattingStyleViewInfo;
begin
  AViewInfo := GetConditionalFormattingStyleViewInfo(ACellViewInfo);
  if AViewInfo <> nil then
  begin
    AViewInfo.ValidateBounds(GetCellBounds(ACellViewInfo));
    AViewInfo.AfterDrawCellBackground(ACanvas, ABackgroundColor);
  end;
end;

procedure TcxDataControllerConditionalFormattingProvider.AfterDrawCellValue(ACellViewInfo: TObject; ACanvas: TcxCanvas);
var
  AViewInfo: TcxDataControllerConditionalFormattingStyleViewInfo;
begin
  AViewInfo := GetConditionalFormattingStyleViewInfo(ACellViewInfo);
  if AViewInfo <> nil then
  begin
    AViewInfo.ValidateBounds(GetCellBounds(ACellViewInfo));
    AViewInfo.AfterDrawCellValue(ACanvas);
  end;
end;

function TcxDataControllerConditionalFormattingProvider.CalculateStyle(
  const AStyle: TdxSpreadSheetCellDisplayStyle; ARow, AColumn: Integer; ACell: IdxSpreadSheetCellData): Boolean;
begin
  Result := ConditionalFormatting.CalculateStyle(AStyle, ARow, AColumn, ACell);
end;

procedure TcxDataControllerConditionalFormattingProvider.CalculateCellEditorBounds(ACellViewInfo: TObject; var R: TRect);
var
  AViewInfo: TcxDataControllerConditionalFormattingStyleViewInfo;
begin
  AViewInfo := GetConditionalFormattingStyleViewInfo(ACellViewInfo);
  if AViewInfo <> nil then
  begin
    AViewInfo.ValidateBounds(GetCellBounds(ACellViewInfo));
    R.Left := AViewInfo.TextRect.Left;
  end;
end;

procedure TcxDataControllerConditionalFormattingProvider.CalculateStyleViewInfo(ACellViewInfo: TObject);
var
  AViewInfo: TcxDataControllerConditionalFormattingStyleViewInfo;
  R: TRect;
begin
  AViewInfo := GetConditionalFormattingStyleViewInfo(ACellViewInfo);
  if AViewInfo <> nil then
  begin
    R := GetCellBounds(ACellViewInfo);
    AViewInfo.Calculate(R);
  end;
end;

procedure TcxDataControllerConditionalFormattingProvider.CanDrawCellValue(ACellViewInfo: TObject; var Allow: Boolean);
var
  AStyle: TdxSpreadSheetCellDisplayStyle;
begin
  if IsActive then
  begin
    if TryGetStyle(EditCellViewInfoToPoint(ACellViewInfo), AStyle) and (AStyle <> nil) then
      Allow := AStyle.ShowCellValue;
  end;
end;

function TcxDataControllerConditionalFormattingProvider.FormatDisplayValue(const ACellViewInfo: TObject; const AValue: Variant): Variant;
var
  AStyle: TdxSpreadSheetCellDisplayStyle;
  ADataCell: IdxSpreadSheetCellData;
  AResult: TdxSpreadSheetNumberFormatResult;
  APoint: TPoint;
begin
  Result := AValue;
  if not IsActive or VarIsEmpty(Result) or VarIsNull(Result) or
      not ConditionalFormatting.IsValueFormattingSupported then
    Exit;
  APoint := EditCellViewInfoToPoint(ACellViewInfo);

  if TryGetStyle(APoint, AStyle) and (AStyle <> nil) then
  begin
    ADataCell := GetCellData(APoint.Y, APoint.X);
    try
      if ADataCell.DataType in [cdtInteger, cdtFloat, cdtCurrency, cdtDateTime] then
      begin
        AStyle.DataFormat.Format(Result, ADataCell.DataType, AResult);
        Result := AResult.Text;
      end;
    finally
      ADataCell := nil;
    end;
  end;
end;

class function TcxDataControllerConditionalFormattingProvider.CalculateNewIndex(ACheckIndex, AOldIndex, ANewIndex: Integer): Integer;
begin
  Result := ACheckIndex;
  if AOldIndex = Result then
    Result := ANewIndex
  else
  if (ANewIndex < AOldIndex) and (Result >= ANewIndex) and (Result <= AOldIndex) then
    Inc(Result)
  else
  if (ANewIndex > AOldIndex) and (Result <= ANewIndex) and (Result >= AOldIndex) then
    Dec(Result);
end;

procedure TcxDataControllerConditionalFormattingProvider.CalculateViewParams(var AParams: TcxViewParams; ARecordIndex, AItemIndex: Integer);
var
  AStyle: TdxSpreadSheetCellDisplayStyle;
  AFound: Boolean;
  ACell: IdxSpreadSheetCellData;
  AKey: TCellStyleCacheKey;
  P: TPoint;
  AStyleChanged: Boolean;
begin
  if not IsActive then
    Exit;

  P := TPoint.Create(AItemIndex, ARecordIndex);
  AKey := TRect.Create(AParams.Font.Handle, AParams.TextColor, AParams.Color, 0);

  AFound := FStyleCache.TryGetCellStyle(P, AKey, AStyle);
  if AFound and (AStyle = nil) then
    Exit;

  AStyleChanged := False;
  try
    if AStyle = nil then
    begin
      AStyle := TdxSpreadSheetCellDisplayStyle.Create(Self);
      AStyle.Font.Assign(AParams.Font);
      AStyle.Font.Color := AParams.TextColor;
      AStyle.Brush.BackgroundColor := AParams.Color;
      ACell := GetCellData(ARecordIndex, AItemIndex);
      AStyleChanged := CalculateStyle(AStyle, ARecordIndex, AItemIndex, ACell);
    end;
  finally
    ACell := nil;
    if AFound or AStyleChanged then
    begin
      AParams.Font := TdxSpreadSheetCellFontAccess(AStyle.Font).Handle.GraphicObject;
      AParams.TextColor := AStyle.Font.Color;
      if AStyle.Brush.BackgroundColor <> clNone then
        AParams.Color := AStyle.Brush.BackgroundColor;
      if not AFound then
        FStyleCache.AddStyle(P, AKey, AStyle);
    end
    else
    begin
      FStyleCache.AddStyle(P, AKey, nil);
      AStyle.Free;
    end;
  end;
end;

function TcxDataControllerConditionalFormattingProvider.IsActive: Boolean;
begin
  Result := not FIsDestroying and (ConditionalFormatting <> nil) and
    (ConditionalFormatting.RuleCount > 0);
end;

function TcxDataControllerConditionalFormattingProvider.IsDetailMode: Boolean;
begin
  Result := False;
end;

procedure TcxDataControllerConditionalFormattingProvider.Recalculate;
begin
  if not IsActive then
    Exit;
  BeginUpdate;
  try
    ClearCache;
  finally
    EndUpdate;
  end;
end;

function TcxDataControllerConditionalFormattingProvider.TryGetStyle(const ACell: TPoint; out AStyle: TdxSpreadSheetCellDisplayStyle): Boolean;
begin
  Result := FStyleCache.TryGetStyle(ACell, AStyle);
end;

function TcxDataControllerConditionalFormattingProvider.CalculateBestFitWidth(const ACell: TPoint; ABaseWidth: Integer): Integer;
var
  AViewInfo: TcxDataControllerConditionalFormattingStyleViewInfo;
begin
  Result := ABaseWidth;
  if FStyleViewInfoCache.TryGetValue(ACell, AViewInfo) and (AViewInfo <> nil) and (AViewInfo.IconBounds.Width > 0) then
    Result := Result + Abs(AViewInfo.IconBounds.Left - AViewInfo.TextRect.Left);
end;

function TcxDataControllerConditionalFormattingProvider.CreateCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := TdxSpreadSheetCellStyles.Create;
end;

function TcxDataControllerConditionalFormattingProvider.CreateConditionalFormatting: TcxDataControllerConditionalFormatting;
begin
  Result := TcxDataControllerConditionalFormatting.Create(Self);
end;

function TcxDataControllerConditionalFormattingProvider.CreateFormatSettings: TcxDataControllerConditionalFormattingFormatSettings;
begin
  Result := TcxDataControllerConditionalFormattingFormatSettings.Create;
end;

function TcxDataControllerConditionalFormattingProvider.CreateFormulaController: TcxDataControllerConditionalFormattingFormulaController;
begin
  Result := TcxDataControllerConditionalFormattingFormulaController.Create(Self);
end;

procedure TcxDataControllerConditionalFormattingProvider.ForEachRow(ARow, AStart, AFinish: Integer; AProc: TdxSpreadSheetViewForEachCellProc; AGoForward: Boolean);
var
  I: Integer;
  ACell: IdxSpreadSheetCellData;
begin
  I := IfThen(AGoForward, AStart, AFinish);
  repeat
    ACell := GetCellData(ARow, I);
    try
      if ACell <> nil then
        AProc(ACell);
    finally
      ACell := nil;
    end;
    I := I + IfThen(AGoForward, 1, -1);
  until (AGoForward and (I > AFinish)) or (not AGoForward and (I < AStart));
end;

procedure TcxDataControllerConditionalFormattingProvider.ClearCache;
begin
  if IsUpdateLocked then
    Exit;
  ClearCacheCore;
end;

procedure TcxDataControllerConditionalFormattingProvider.ClearCacheCore;
begin
  FStyleCache.Clear;
  FStyleViewInfoCache.Clear;
  ConditionalFormatting.FlushCache;
end;

procedure TcxDataControllerConditionalFormattingProvider.ConditionalFormattingChanged;
begin
  if IsDestroying or IsOwnerLocked then
    Exit;
  BeginUpdate;
  try
    ClearCache;
  finally
    EndUpdate;
  end;
  SetDesignerModified(GetOwner);
end;

procedure TcxDataControllerConditionalFormattingProvider.Changed;
begin
// do nothing
end;

function TcxDataControllerConditionalFormattingProvider.DoGetCellValue(ARecordIndex, AItemIndex: Integer): Variant;
begin
  Result := DataController.Values[ARecordIndex, AItemIndex];
end;

function TcxDataControllerConditionalFormattingProvider.FindFieldByItem(AItem: TObject): Integer;
var
  AField: TcxCustomDataField;
begin
  AField := TcxCustomDataControllerAccess(DataController).Fields.FieldByItem(AItem);
  if AField <> nil then
    Result := AField.Index
  else
    Result := -1;
end;

function TcxDataControllerConditionalFormattingProvider.FindFieldByName(const AName: string): Integer;
var
  ADataController: TcxCustomDataControllerAccess;
  AField: TcxCustomDataField;
  S: string;
  I: Integer;
begin
  ADataController := TcxCustomDataControllerAccess(DataController);
  for I := 0 to ADataController.Fields.Count - 1 do
  begin
    AField := ADataController.Fields[I];
    if AField.Item = nil then
      Continue;
    S := GetDisplayName(AField);
    if AnsiSameText(AName, S) then
      Exit(AField.Index);
  end;
  Result := -1;
end;

function TcxDataControllerConditionalFormattingProvider.GetCellValue(ARecordIndex, AItemIndex: Integer): Variant;
var
  AResult: Variant;
  AEditProperties: TcxCustomEditProperties;
begin
  Result := DoGetCellValue(ARecordIndex, AItemIndex);
  AEditProperties := GetEditProperties(DataController.GetItem(AItemIndex));
  if (AEditProperties <> nil) and (AEditProperties is TcxCustomTextEditProperties) then
  begin
    if TcxCustomTextEditPropertiesAccess(AEditProperties).IsLookupEdit then
    begin
      TcxCustomTextEditProperties(AEditProperties).PrepareDisplayValue(Result, AResult, False);
      Result := AResult;
    end;
  end;
end;

function TcxDataControllerConditionalFormattingProvider.GetConditionalFormattingStyleViewInfo(ACell: TObject): TcxDataControllerConditionalFormattingStyleViewInfo;
var
  AStyle: TdxSpreadSheetCellDisplayStyle;
  APoint: TPoint;
begin
  Result := nil;

  if not IsActive then
    Exit;
  APoint := EditCellViewInfoToPoint(ACell);

  if StyleViewInfoCache.TryGetValue(APoint, Result) then
    Exit;
  if TryGetStyle(APoint, AStyle) and (AStyle <> nil) then
  begin
    Result := TcxDataControllerConditionalFormattingStyleViewInfo.Create(AStyle, GetScaleFactor);
    StyleViewInfoCache.Add(APoint, Result);
  end
  else
    StyleViewInfoCache.Add(APoint, nil);
end;

function TcxDataControllerConditionalFormattingProvider.GetRecordCount: Integer;
begin
  Result := FDataController.RecordCount;
end;

function TcxDataControllerConditionalFormattingProvider.IsDesigning: Boolean;
begin
  Result := csDesigning in GetOwner.ComponentState;
end;

function TcxDataControllerConditionalFormattingProvider.IsUpdateLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

function TcxDataControllerConditionalFormattingProvider.ReferenceToString(AColumn: Integer): string;
begin
  if FormatSettings.NativeReferences then
    Result := Format('%0:s%1:s%0:s', [TdxSpreadSheetColumnHelper.NameByIndex(AColumn), dxAreaSeparator])
  else
    Result := Format('[%s]', [ReferenceToString(cxRect(AColumn, 0, AColumn, 0))]);
end;

function TcxDataControllerConditionalFormattingProvider.ReferenceToString(const AArea: TRect): string;
var
  ABuffer: TStringBuilder;
  ADataController: TcxCustomDataControllerAccess;
  AName: string;
  ARight: Integer;
  AStart: Integer;
  I: Integer;
begin
  if FormatSettings.NativeReferences then
    Exit(dxReferenceToString(AArea));

  Result := '';
  ADataController := TcxCustomDataControllerAccess(DataController);
  ARight := Min(ADataController.Fields.Count - 1, AArea.Right);
  AStart := Max(0, AArea.Left);
  if AStart <= ARight then
  begin
    ABuffer := TdxStringBuilderManager.Get;
    try
      for I := AStart to ARight do
      begin
        AName := GetDisplayName(ADataController.Fields[I]);
        if ABuffer.Length > 0 then
          ABuffer.Append(', ');
        ABuffer.Append(AName);
      end;
      Result := ABuffer.ToString;
    finally
      TdxStringBuilderManager.Release(ABuffer);
    end;
  end;
end;

function TcxDataControllerConditionalFormattingProvider.IsStoringSupported: Boolean;
var
  AOwner: TComponent;
  AIntf: IcxDataControllerConditionalFormattingProviderOwner;
begin
  AOwner := GetOwner;
  Result := Supports(AOwner, IcxDataControllerConditionalFormattingProviderOwner, AIntf) and
    (AIntf.GetConditionalFormattingProvider = Self);
end;

procedure TcxDataControllerConditionalFormattingProvider.DefineBinaryProperty(AFiler: TFiler);

  function HasData: Boolean;
  var
    AProvider: TcxDataControllerConditionalFormattingProvider;
    AStream1, AStream2: TMemoryStream;
    AIntf: IcxDataControllerConditionalFormattingProviderOwner;
  begin
    if AFiler.Ancestor = nil then
      Result := IsActive
    else
    begin
      Result := Supports(AFiler.Ancestor, IcxDataControllerConditionalFormattingProviderOwner, AIntf);
      if Result then
      begin
        AProvider := AIntf.GetConditionalFormattingProvider;
        if AProvider = nil then
          Result := IsActive
        else
        begin
          Result := AProvider.IsActive xor IsActive;
          if not Result and IsActive then
          begin
            AStream1 := TMemoryStream.Create;
            AStream2 := TMemoryStream.Create;
            try
              Write(AStream1);
              AProvider.Write(AStream2);
              Result := not StreamsEqual(AStream1, AStream2);
            finally
              AStream2.Free;
              AStream1.Free;
            end;
          end;
        end;
      end;
    end;
  end;

begin
  if IsStoringSupported then
    AFiler.DefineBinaryProperty('ConditionalFormatting', Read, Write, HasData);
end;

procedure TcxDataControllerConditionalFormattingProvider.LoadFromStream(AStream: TStream);
var
  AReader: TcxReader;
  ACount: Integer;
  AClass: TPersistentClass;
  AClassName: string;
  ARule: TdxSpreadSheetConditionalFormattingCustomRule;
  ASize: Integer;
begin
  StoringBegin;
  BeginUpdate;
  try
    ConditionalFormatting.Clear;
    AReader := TcxReader.Create(AStream);
    try
      ACount := AReader.ReadInteger;
      AReader.Version := AReader.ReadInteger;
      while ACount > 0 do
      begin
        AClassName := AReader.ReadWideString;
        ASize := AReader.ReadInteger;
        AClass := GetClass(AClassName);
        if (AClass <> nil) and AClass.InheritsFrom(TdxSpreadSheetConditionalFormattingCustomRule) then
        begin
          ConditionalFormatting.Add(TdxSpreadSheetConditionalFormattingCustomRuleClass(AClass), ARule);
          ARule.LoadFromStream(AReader);
        end
        else
          AReader.Stream.Seek(ASize, soCurrent);
        Dec(ACount);
      end;
    finally
      AReader.Free;
    end;
  finally
    EndUpdate;
    StoringEnd;
  end;
end;

procedure TcxDataControllerConditionalFormattingProvider.Loaded;
begin
  if (FLoadingStream <> nil) and (FLoadingStream.Size > 0) then
  begin
    FLoadingStream.Position := 0;
    LoadFromStream(FLoadingStream);
    FreeAndNil(FLoadingStream);
  end;
end;

procedure TcxDataControllerConditionalFormattingProvider.Read(AStream: TStream);
begin
  if GetOwner.ComponentState * [csReading, csLoading] <> [] then
  begin
    if FLoadingStream = nil then
      FLoadingStream := TMemoryStream.Create
    else
      FLoadingStream.Clear;
    FLoadingStream.LoadFromStream(AStream)
  end
  else
    LoadFromStream(AStream);
end;

procedure TcxDataControllerConditionalFormattingProvider.Write(AStream: TStream);
const
  Version = 10;
var
  APosRule: Integer;
  APosSize: Integer;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  ASize: Integer;
  AWriter: TcxWriter;
  I: Integer;
begin
  StoringBegin;
  try
    AWriter := TcxWriter.Create(AStream);
    try
      AWriter.WriteInteger(ConditionalFormatting.RuleCount);
      AWriter.WriteInteger(Version);
      for I := 0 to ConditionalFormatting.RuleCount - 1 do
      begin
        ARule := ConditionalFormatting.Rules[I];
        AWriter.WriteWideString(ARule.ClassName);
        APosSize := AWriter.Stream.Position;
        AWriter.WriteInteger(0);
        APosRule := AWriter.Stream.Position;
        ARule.SaveToStream(AWriter);
        ASize := AWriter.Stream.Position - APosRule;
        AWriter.Stream.Seek(APosSize, soBeginning);
        AWriter.WriteInteger(ASize);
        AWriter.Stream.Seek(ASize, soCurrent);
      end;
    finally
      AWriter.Free;
    end;
  finally
    StoringEnd;
  end;
end;

procedure TcxDataControllerConditionalFormattingProvider.BeginUpdateFieldsOrder;
begin
  Inc(FFieldsOrderLockCount);
end;

procedure TcxDataControllerConditionalFormattingProvider.EndUpdateFieldsOrder;
begin
  Dec(FFieldsOrderLockCount);
end;

procedure TcxDataControllerConditionalFormattingProvider.ForEachCell(const AArea: TRect; AProc: TdxSpreadSheetViewForEachCellProc; AGoForward: Boolean = True);
var
  I, AFirstRowIndex, ALastRowIndex, ARecordIndex: Integer;
  AFirstItem, ALastItem: Integer;
  ARecordCount: Integer;
begin
  if ConditionalFormatting.ExcludeFilteredOutRecords then
    ARecordCount := DataController.FilteredRecordCount - 1
  else
    ARecordCount := GetMaxRowIndex;
  AFirstRowIndex := Min(ARecordCount, Max(0, AArea.Top));
  ALastRowIndex := Min(ARecordCount, Max(0, AArea.Bottom));
  AFirstItem := Min(GetMaxColumnIndex, Max(0, AArea.Left));
  ALastItem := Min(GetMaxColumnIndex, Max(0, AArea.Right));
  I := IfThen(AGoForward, AFirstRowIndex, ALastRowIndex);
  repeat
    if ConditionalFormatting.ExcludeFilteredOutRecords then
      ARecordIndex := DataController.FilteredRecordIndex[I]
    else
      ARecordIndex := I;
    ForEachRow(ARecordIndex, AFirstItem, ALastItem, AProc, AGoForward);
    I := I + IfThen(AGoForward, 1, -1);
  until (AGoForward and (I > ALastRowIndex)) or (not AGoForward and (I < AFirstRowIndex));
end;

function TcxDataControllerConditionalFormattingProvider.GetCellData(const ARow, AColumn: Integer): IdxSpreadSheetCellData;
begin
  Result := nil;
  if (ARow < 0) or (ARow > GetMaxRowIndex) then
    Exit;
  if (AColumn < 0) or (AColumn > GetMaxColumnIndex) then
    Exit;
  if ConditionalFormatting.ExcludeFilteredOutRecords and (DataController.FilteredIndexByRecordIndex[ARow] = -1) then
    Exit;
  Result := TcxDataControllerConditionalFormattingCellData.Create(Self, ARow, AColumn);
end;

function TcxDataControllerConditionalFormattingProvider.GetMaxColumnIndex: Integer;
begin
  Result := TcxCustomDataControllerAccess(FDataController).Fields.Count - 1;
end;

function TcxDataControllerConditionalFormattingProvider.GetMaxRowIndex: Integer;
begin
  Result := GetRecordCount;
  Dec(Result);
end;

function TcxDataControllerConditionalFormattingProvider.GetNextColumnWithNonEmptyCell(const ARow, AColumn: Integer;
  const AGoForward: Boolean = True): Integer;
begin
  Result := MaxInt;
end;

function TcxDataControllerConditionalFormattingProvider.GetNextRowWithNonEmptyCell(const ARow, AColumn: Integer;
  const AGoForward: Boolean = True): Integer;
const
  AIncrement: array[Boolean] of Integer = (-1, 1);
var
  ACell: IdxSpreadSheetCellData;
begin
  Result := ARow + AIncrement[AGoForward];
  while (AGoForward and (Result < FDataController.RowCount)) or (not AGoForward and (Result >= 0)) do
  begin
    ACell := GetCellData(Result, AColumn);
    if (ACell <> nil) and not ACell.IsEmpty then
      Break;
    Inc(Result, AIncrement[AGoForward]);
  end;
end;

procedure TcxDataControllerConditionalFormattingProvider.SetCellData(const ARow, AColumn: Integer; const AValue: Variant; const AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
// do nothing
end;

function TcxDataControllerConditionalFormattingProvider.GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
begin
  Result := FFormulaController.FormatSettings;
end;

procedure TcxDataControllerConditionalFormattingProvider.CellStyleChanged;
begin
// do nothing
end;

procedure TcxDataControllerConditionalFormattingProvider.CellStyleChanging;
begin
// do nothing
end;

function TcxDataControllerConditionalFormattingProvider.GetCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := FCellStyles;
end;

procedure TcxDataControllerConditionalFormattingProvider.ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);
begin
  // do nothing
end;

function TcxDataControllerConditionalFormattingProvider.GetFormulaController: TdxSpreadSheetCustomFormulaController;
begin
  Result := FFormulaController;
end;

procedure TcxDataControllerConditionalFormattingProvider.BeginUpdate;
begin
  if not IsUpdateLocked then
    DoBeginUpdate;
  Inc(FLockCount);
end;

procedure TcxDataControllerConditionalFormattingProvider.EndUpdate;
begin
  Dec(FLockCount);
  if not IsUpdateLocked then
  begin
    ClearCache;
    Changed;
    DoEndUpdate;
  end;
end;

function TcxDataControllerConditionalFormattingProvider.GetSelectionArea: TRect;
var
  AItem: TObject;
begin
  if not FCustomSelectedArea.IsEqual(cxNullRect) then
    Exit(FCustomSelectedArea);
  Result := DefaultArea;
  AItem := GetFocusedItem;
  if IsDesigning or (AItem = nil) then
    Exit;
  Result.Left := FindFieldByItem(AItem);
  Result.Right := Result.Left;
  Result.Top := 0;
  Result.Bottom := MaxInt;
end;

function TcxDataControllerConditionalFormattingProvider.GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
begin
  Result := FConditionalFormatting;
end;

function TcxDataControllerConditionalFormattingProvider.GetParentForm: TCustomForm;
begin
  if IsDesigning then
    Result := nil
  else
    Result := DoGetParentForm;
end;

function TcxDataControllerConditionalFormattingProvider.CreateChild(const AObjectName, AClassName: string): TObject;
var
  AClass: TdxSpreadSheetCustomConditionalFormattingRuleClass;
begin
  Result := nil;
  AClass := TdxSpreadSheetCustomConditionalFormattingRuleClass(FindClass(AClassName));
  if AClass <> nil then
    ConditionalFormatting.Add(AClass, Result);
end;

procedure TcxDataControllerConditionalFormattingProvider.DeleteChild(const AObjectName: string; AObject: TObject);
var
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
begin
  if AObject is TdxSpreadSheetCustomConditionalFormattingRule then
  begin
    ARule := TdxSpreadSheetCustomConditionalFormattingRule(AObject);
    ConditionalFormatting.Remove(ARule);
  end;
end;

procedure TcxDataControllerConditionalFormattingProvider.GetChildren(AChildren: TStringList);
var
  I: Integer;
  AIntf: IcxStoredObject;
begin
  if not IsStoringSupported then
    Exit;
  for I := 0 to ConditionalFormatting.RuleCount - 1 do
  begin
    AIntf := ConditionalFormatting[I] as IcxStoredObject;
    AChildren.AddObject(AIntf.GetObjectName, ConditionalFormatting[I]);
  end;
end;

procedure TcxDataControllerConditionalFormattingProvider.StoringBegin;
begin
  ExchangePointers(FFormatSettings, FFormatSettingsInvariant);
end;

procedure TcxDataControllerConditionalFormattingProvider.StoringEnd;
begin
  ExchangePointers(FFormatSettings, FFormatSettingsInvariant);
end;

function TcxDataControllerConditionalFormattingProvider.GetObjectName: string;
begin
  Result := 'ConditionalFormattingProvider';
end;

function TcxDataControllerConditionalFormattingProvider.GetProperties(AProperties: TStrings): Boolean;
begin
  Result := IsStoringSupported;
  if not Result then
    Exit;
  AProperties.Add('Count');
end;

procedure TcxDataControllerConditionalFormattingProvider.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Count' then
    AValue := ConditionalFormatting.RuleCount;
end;

procedure TcxDataControllerConditionalFormattingProvider.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Count' then
    ConditionalFormatting.Clear;
end;

function TcxDataControllerConditionalFormattingProvider.CanDelete: Boolean;
begin
  Result := False;
end;

function TcxDataControllerConditionalFormattingProvider.IsOwnerLocked: Boolean;
begin
  Result := GetOwner.ComponentState * [csReading, csLoading, csDestroying] <> [];
end;

procedure TcxDataControllerConditionalFormattingProvider.FieldIndexChangedHandler(Sender: TObject; AOldIndex, ANewIndex: Integer);

  procedure UpdateToken(AToken: TdxSpreadSheetFormulaToken);
  var
    I: Integer;
  begin
    while AToken <> nil do
    begin
      if AToken is TcxDataControllerConditionalFormattingFormulaFieldReference then
        TcxDataControllerConditionalFormattingFormulaFieldReference(AToken).FieldIndexChanged(AOldIndex, ANewIndex);
      if AToken.HasChildren then
        for I := 0 to AToken.ChildCount - 1 do
          UpdateToken(AToken.Items[I]);
      AToken := AToken.Next;
    end;
  end;

  procedure UpdateFieldIndex(AExpression: TdxSpreadSheetConditionalFormattingExpression);
  var
    AToken: TdxSpreadSheetFormulaToken;
  begin
    if AExpression = nil then
      Exit;
    AToken := AExpression.Tokens;
    UpdateToken(AToken);
  end;

var
  I: Integer;
  AArea: TRect;
  AWidth: Integer;
  ARule: TdxSpreadSheetConditionalFormattingRuleExpressionAccess;
begin
  if not IsActive or IsOwnerLocked then
    Exit;
  BeginUpdate;
  try
    for I := 0 to ConditionalFormatting.RuleCount - 1 do
    begin
      if ConditionalFormatting[I] is TdxSpreadSheetConditionalFormattingRuleExpression then
      begin
        ARule := TdxSpreadSheetConditionalFormattingRuleExpressionAccess(ConditionalFormatting[I]);
        UpdateFieldIndex(ARule.Formulas[0]);
        UpdateFieldIndex(ARule.Formulas[1]);
      end;
      if FFieldsOrderLockCount > 0 then
        Continue;
      AArea := ConditionalFormatting[I].Area;
      AWidth := AArea.Width;
      if AWidth = MaxInt then
        Continue;
      AArea.Left := CalculateNewIndex(AArea.Left, AOldIndex, ANewIndex);
      AArea.Width := Min(AWidth, MaxInt);
      ConditionalFormatting[I].Area := AArea;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TcxDataControllerConditionalFormattingProvider.DataControllerChangedHandler(Sender: TObject);
begin
  if (Sender = FDataController) and not IsOwnerLocked then
    Recalculate;
end;

{ TcxDataControllerConditionalFormatting }

constructor TcxDataControllerConditionalFormatting.Create(AOwner: TcxDataControllerConditionalFormattingProvider);
begin
  inherited Create(AOwner);
  FProvider := AOwner;
end;

procedure TcxDataControllerConditionalFormatting.Add(const AField: TcxCustomDataField; ARuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass; out ARule);
begin
  Add(AField.Index, ARuleClass, ARule);
end;

procedure TcxDataControllerConditionalFormatting.Add(const AFieldIndex: Integer; ARuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass; out ARule);
begin
  Add(cxRect(AFieldIndex, 0, AFieldIndex, MaxInt), ARuleClass, ARule);
end;

procedure TcxDataControllerConditionalFormatting.Add(const AFieldName: string; ARuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass; out ARule);
var
  AIndex: Integer;
begin
  AIndex := Provider.FindFieldByName(AFieldName);
  if AIndex < 0 then
    raise EInvalidArgument.CreateFmt(cxGetResourceString(@sdxErrorFieldNotFound), [AFieldName]);
  Add(AIndex, ARuleClass, ARule)
end;

function TcxDataControllerConditionalFormatting.GetFieldDisplayName(AField: TcxCustomDataField): string;
begin
  Result := Provider.GetDisplayName(AField);
end;

function TcxDataControllerConditionalFormatting.IsFieldVisible(AField: TcxCustomDataField): Boolean;
begin
  Result := Provider.IsFieldVisible(AField);
end;

function TcxDataControllerConditionalFormatting.ReferencesToString(const AAreas: TdxSpreadSheetAreaList): string;
var
  AArea: TRect;
begin
  if AAreas.Count > 0 then
    AArea := AAreas.First
  else
    AArea := cxNullRect;

  Result := Provider.ReferenceToString(AArea);
end;

function TcxDataControllerConditionalFormatting.CanShowRulesManagerDialog: Boolean;
begin
  Result := TcxDataControllerConditionalFormattingRulesManagerDialogProvider.CanShowRulesManagerDialog and
    (DataController.ItemCount > 0);
end;

function TcxDataControllerConditionalFormatting.CanValidateExpressionRuleResultValue: Boolean;
begin
  Result := False;
end;

procedure TcxDataControllerConditionalFormatting.ShowRulesManagerDialog;
begin
  TcxDataControllerConditionalFormattingRulesManagerDialogProvider.ShowRulesManagerDialog(Provider);
end;

procedure TcxDataControllerConditionalFormatting.DoChanged;
begin
  Provider.ConditionalFormattingChanged;
end;

function TcxDataControllerConditionalFormatting.IsStyleBorderSupported: Boolean;
begin
  Result := False;
end;

function TcxDataControllerConditionalFormatting.IsValueFormattingSupported: Boolean;
begin
  Result := False;
end;

function TcxDataControllerConditionalFormatting.GetFormulaEditMask: string;
const
  AStringChooser = '\"([^\"])+\"';

  function ReplaceSpecialSymbols(const S: string): string;
  const
    ARegExpSpecialSymbols: string = '''';
  var
    I: Integer;
  begin
    Result := S;
    for I := 1 to Length(ARegExpSpecialSymbols) do
      Result := StringReplace(Result, ARegExpSpecialSymbols[I], '''\' + ARegExpSpecialSymbols[I] + '''', [rfReplaceAll]);
  end;

var
  AFieldsChooser: string;
  I: Integer;
  S: string;
  ADataController: TcxCustomDataControllerAccess;
begin
  Result := '=.+';
  AFieldsChooser := '';
  ADataController := TcxCustomDataControllerAccess(DataController);
  if ADataController.Fields.Count = 0 then
    Exit;

  for I := 0 to ADataController.Fields.Count - 1 do
  begin
    S := GetFieldDisplayName(ADataController.Fields[I]);
    if Length(S) > 0 then
    begin
      S := ReplaceSpecialSymbols(S);
      S := Format('''%s''', [S]);
      if AFieldsChooser = '' then
        AFieldsChooser := S
      else
        AFieldsChooser := Format('%s|%s', [AFieldsChooser, S]);
    end;
  end;
  if AFieldsChooser = '' then
    Exit;

  AFieldsChooser := Format('\[(%s)\]', [AFieldsChooser]);
  Result := Format('=(([^\[\"\''])|(%0:s)|(%1:s))+', [AStringChooser, AFieldsChooser]);
end;

procedure TcxDataControllerConditionalFormatting.SetCustomAreaInfoCache(const Value: TObjectDictionary<string, TdxSpreadSheetConditionalFormattingAreaInfo>);
begin
  FAreaInfoCache := Value;
  FlushCache;
end;

procedure TcxDataControllerConditionalFormatting.SetCustomProvider(const Value: TcxDataControllerConditionalFormattingProvider);
begin
  FProvider := Value;
  FOwner := Value;
end;

procedure TcxDataControllerConditionalFormatting.SetExcludeFilteredOutRecords(
  const Value: Boolean);
begin
  if FExcludeFilteredOutRecords <> Value then
  begin
    BeginUpdate;
    try
      FExcludeFilteredOutRecords := Value;
    finally
      EndUpdate;
    end;
  end;
end;

function TcxDataControllerConditionalFormatting.GetDataController: TcxCustomDataController;
begin
  Result := Provider.DataController;
end;

{ TcxCustomControlControllerConditionalFormattingProvider }

function TcxCustomControlControllerConditionalFormattingProvider.GetCellBounds(ACellViewInfo: TObject): TRect;
begin
  Result := TcxEditCellViewInfoAccess(ACellViewInfo).ContentRect;
  Result.Location := cxNullPoint;
  Result := cxRectContent(Result, TcxEditCellViewInfoAccess(ACellViewInfo).ViewData.ContentOffset);
end;

function TcxCustomControlControllerConditionalFormattingProvider.GetDisplayName(AField: TcxCustomDataField): string;
begin
  if (AField.Item = nil) or not (AField.Item is TcxCustomInplaceEditContainer) then
    Result := ''
  else
    Result := GetItemDisplayName(TcxCustomInplaceEditContainer(AField.Item));
end;

function TcxCustomControlControllerConditionalFormattingProvider.GetEditProperties(AItem: TObject): TcxCustomEditProperties;
var
  AContainer: TcxCustomInplaceEditContainerAccess absolute AItem;
begin
  if (AContainer <> nil) and (AItem is TcxCustomInplaceEditContainer) then
    Result := AContainer.PropertiesValue
  else
    Result := nil;
end;

function TcxCustomControlControllerConditionalFormattingProvider.GetFocusedItem: TObject;
begin
  Result := Controller.FocusedItem;
end;

function TcxCustomControlControllerConditionalFormattingProvider.IsFieldVisible(AField: TcxCustomDataField): Boolean;
begin
  Result := (AField.Item <> nil) and (AField.Item is TcxCustomInplaceEditContainer) and
    IsItemVisible(TcxCustomInplaceEditContainer(AField.Item));
end;

function TcxCustomControlControllerConditionalFormattingProvider.EditCellViewInfoToPoint(ACellViewInfo: TObject): TPoint;
var
  AEditCell: TcxEditCellViewInfoAccess absolute ACellViewInfo;
begin
  Result := cxPoint(FindFieldByItem(AEditCell.EditContainer), AEditCell.RecordIndex);
end;

{ TcxDataControllerConditionalFormattingRulesManagerDialogProvider }

class {$IFDEF DELPHIXE}destructor{$ELSE}procedure{$ENDIF} TcxDataControllerConditionalFormattingRulesManagerDialogProvider.Finalize;
begin
  FreeAndNil(FRulesManagerDialogs);
end;

class procedure TcxDataControllerConditionalFormattingRulesManagerDialogProvider.RequiresRulesManagerDialogUnits(AClass: TClass; const AProc: TGetStrProc);
var
  I: Integer;
begin
  if (AClass.GetInterfaceEntry(IcxDataControllerConditionalFormattingProviderOwner) <> nil) and
    (FRulesManagerDialogs <> nil) and (RulesManagerDialogs.Count > 0) then
  begin
    for I := 0 to FRulesManagerDialogs.Count - 1 do
      AProc(FRulesManagerDialogs[I].UnitName);
  end;
end;

class procedure TcxDataControllerConditionalFormattingRulesManagerDialogProvider.RequiresRulesManagerDialogUnits(
  const ARootComponent: TComponent; const AProc: TGetStrProc);
var
  I: Integer;
  AOwner: IcxDataControllerConditionalFormattingProviderOwner;
begin
  if (FRulesManagerDialogs = nil) or (RulesManagerDialogs.Count = 0) then
    Exit;
  for I := 0 to ARootComponent.ComponentCount - 1 do
  begin
    if Supports(ARootComponent.Components[I], IcxDataControllerConditionalFormattingProviderOwner, AOwner) then
    try
      if (AOwner.GetConditionalFormattingProvider <> nil) and (AOwner.GetConditionalFormattingProvider.ConditionalFormatting.RuleCount > 0) then
      begin
        TcxDataControllerConditionalFormattingRulesManagerDialogProvider.RequiresRulesManagerDialogUnits(ARootComponent.Components[I].ClassType, AProc);
        Break;
      end;
    finally
      AOwner := nil;
    end;
  end;
end;

class procedure TcxDataControllerConditionalFormattingRulesManagerDialogProvider.RegisterRulesManagerDialog(AAdapter: TAdapterClass);
begin
  RulesManagerDialogs.Insert(0, AAdapter);
end;

class procedure TcxDataControllerConditionalFormattingRulesManagerDialogProvider.UnregisterRulesManagerDialog(AAdapter: TAdapterClass);
begin
  if FRulesManagerDialogs <> nil then
    FRulesManagerDialogs.Remove(AAdapter);
end;

class function TcxDataControllerConditionalFormattingRulesManagerDialogProvider.CanShowRulesManagerDialog: Boolean;
begin
  Result := (FRulesManagerDialogs <> nil) and (FRulesManagerDialogs.Count > 0);
end;

class procedure TcxDataControllerConditionalFormattingRulesManagerDialogProvider.ShowRulesManagerDialog(AProvider: TcxDataControllerConditionalFormattingProvider);
var
  AAdapter: TAdapter;
begin
  if not CanShowRulesManagerDialog then
    Exit;
  AAdapter := RulesManagerDialogs[0].Create;
  try
    AAdapter.Execute(AProvider);
  finally
    AAdapter.Free;
  end;
end;

class function TcxDataControllerConditionalFormattingRulesManagerDialogProvider.GetRulesManagerDialogs: TList<TAdapterClass>;
begin
  if FRulesManagerDialogs = nil then
    FRulesManagerDialogs := TList<TAdapterClass>.Create;
  Result := FRulesManagerDialogs;
end;

initialization
  TcxDataControllerConditionalFormattingProvider.Initialize;

finalization
  TcxDataControllerConditionalFormattingProvider.Finalize;
{$IFNDEF DELPHIXE}
  TcxDataControllerConditionalFormattingRulesManagerDialogProvider.Finalize;
{$ENDIF}

end.
