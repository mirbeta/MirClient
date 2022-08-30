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

unit dxRichEdit.DocumentModel.Fields;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, Rtti,

  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Options.Simple,
  dxRichEdit.Options,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Token,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.DateTimeFieldFormatter,
  dxRichEdit.DocumentModel.MailMerge;

type
  TdxCalculateFieldResult = class;
  TdxUpdateFieldOperationResult = class;

  TdxUpdateFieldResult = (FieldNotUpdated, FieldUpdated, FieldUpdatedAndCodeDeleted);

  { IdxFieldDataService }

  IdxFieldDataService = interface
  ['{0983EB67-0CB6-4C11-97B9-6D00EA737BDF}']
    function GetBoundMode: Boolean;
    function GetFieldValue(AMailMergeProperties: TdxMailMergeProperties; const AFieldName: string;
      AMapFieldName: Boolean; AOptions: TdxMailMergeDataMode; APieceTable: TdxCustomPieceTable;
      AField: TdxField): TValue;
    property BoundMode: Boolean read GetBoundMode;
  end;

  { TdxDocumentLogInterval }

  TdxDocumentLogInterval = record
  strict private
    FStart: TdxDocumentLogPosition;
    FLength: Integer;
  public
    constructor Create(AStart: TdxDocumentLogPosition; ALength: Integer);

    class function Null: TdxDocumentLogInterval; static;
    function IsNull: Boolean;
    function IsValid: Boolean;

    property Start: TdxDocumentLogPosition read FStart;
    property Length: Integer read FLength;
  end;
  TdxDocumentLogIntervalList = class(TList<TdxDocumentLogInterval>);

  { IdxFieldCalculatorService }

  IdxFieldCalculatorService = interface
  ['{E3970474-C4A8-404F-8601-B9194C573D4C}']
    function CalculateField(APieceTable: TdxCustomPieceTable; AField: TdxField;
      AMailMergeDataMode: TdxMailMergeDataMode; AUpdateType: TdxUpdateFieldOperationType): TdxCalculateFieldResult;
    procedure PrepareField(APieceTable: TdxCustomPieceTable; AField: TdxField; AUpdateType: TdxUpdateFieldOperationType);
    procedure BeginUpdateFieldsOnLoad(AOptions: TdxFieldUpdateOnLoadOptions);
    procedure EndUpdateFieldsOnLoad;
  end;

  { TdxInstructionCollection }

  TdxInstructionCollection = class
  strict private
    FSwitches: TdxNamedObjectDictionary<TdxTokenList>;
    FSwitchIntervals: TdxNamedObjectDictionary<TdxDocumentLogIntervalList>;
    FArguments: TdxTokenList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSwitch(const AFieldSwitch: string; const AInterval: TdxDocumentLogInterval); overload;
    procedure AddSwitch(const AFieldSwitch: string; const AInterval: TdxDocumentLogInterval;
      AFieldArgument: IdxToken); overload;
    procedure AddArgument(const AFieldArgument: IdxToken);
    function GetInt(const AFieldSwitch: string): Integer;
    function GetString(const AFieldSwitch: string): TdxNullableString;
    function GetStringArray(const AFieldSwitch: string): TArray<string>;
    function GetBool(const AFieldSwitch: string): Boolean;
    function GetNullableInt(const AFieldSwitch: string): TdxNullableInteger;
    function GetDefaultToken(const ATokens: TdxTokenList): IdxToken;
    function GetDefaultInterval(AIntervals: TdxDocumentLogIntervalList): TdxDocumentLogInterval;
    function GetArgument(AIndex: Integer): IdxToken;
    function GetArgumentAsString(AIndex: Integer): string;
    function GetArgumentAsDocumentInterval(AIndex: Integer): TdxDocumentLogInterval;
    function GetSwitchArgumentDocumentInterval(const AFieldSwitch: string): TdxDocumentLogInterval; overload;
    function GetSwitchArgumentDocumentInterval(const AFieldSwitch: string;
      AIncludeFullInterval: Boolean): TdxDocumentLogInterval; overload;
    function GetSwitchDocumentInterval(const AFieldSwitch: string): TdxDocumentLogInterval;

    property Arguments: TdxTokenList read FArguments;
    property Switches: TdxNamedObjectDictionary<TdxTokenList> read FSwitches;
    property SwitchIntervals: TdxNamedObjectDictionary<TdxDocumentLogIntervalList> read FSwitchIntervals;
  end;

  { TdxCalculatedFieldBase }

  TdxFieldMailMergeType = (
    NonMailMerge = 1,
    MailMerge = 2,
    Mixed = 3);

  TdxCalculatedFieldBase = class abstract(TdxCustomCalculatedField)
  public const
    FrameworkStringFormatSwitch = '$';
    MergeFormatKeyword = 'MERGEFORMAT';
    MergeFormatInetKeyword = 'MERGEFORMATINET';
    CharFormatKeyword = 'CHARFORMAT';
  strict private
    FCommonStringFormat: TdxNullableString;
    FDateAndTimeFormatting: TdxNullableString;
    FGeneralFormatting: TArray<string>;
    FNumericFormatting: TdxNullableString;
    FSwitches: TdxInstructionCollection;
    FUseCurrentCultureDateTimeFormat: Boolean;
  protected
    class function CreateSwitchesWithArgument(const ASwitches: TArray<string>): TdxStringBooleanDictionary; overload; static;
    class function CreateSwitchesWithArgument: TdxStringBooleanDictionary; overload; static;
    class function GetDefaultMailMergeFieldText(const AFieldName: string): string; static;
    function GetFieldTypeName: string; virtual; abstract;
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; virtual; abstract;
    function GetCanPrepare: Boolean; virtual;
    function GetAllowAsyncUpdate: Boolean; virtual;
    function MailMergeType: TdxFieldMailMergeType; virtual;
    function InsertDefaultMailMergeText(ASourcePieceTable: TdxCustomPieceTable): Boolean; virtual;
    function GetCharacterFormatFlag: TdxFieldResultOptions; virtual;
    function GetSholdApplyFormating: Boolean; virtual;
    function TryApplyCommonStringFormat(AValue: TdxCalculatedFieldValue): Boolean;
    function CreateDateTimeFieldFormatter: TdxDateTimeFieldFormatter; virtual;
    function GetDocumentModelStringValue(ADocumentModel: TdxCustomDocumentModel): string; virtual;
    function GetObjectForFormat(AValue: TdxCalculatedFieldValue; const ACommonStringFormat: string): TValue;
    function TryApplyDateAndTimeFormatting(AValue: TdxCalculatedFieldValue): Boolean; virtual;
    function TryApplyNumericFormatting(AValue: TdxCalculatedFieldValue;
      ACustomSeparators: TdxMailMergeCustomSeparators): Boolean; virtual;
    function TryConvertToDateTime(AValue: TdxCalculatedFieldValue; out AResult: TDateTime): Boolean; virtual;
    function TryConvertToDouble(AValue: TdxCalculatedFieldValue; out AResult: Double): Boolean; virtual;
    function IsEmptyString(AValue: TValue): Boolean;
    property UseCurrentCultureDateTimeFormat: Boolean read FUseCurrentCultureDateTimeFormat;
    property FieldTypeName: string read GetFieldTypeName;
    property SwitchesWithArguments: TdxStringBooleanDictionary read GetSwitchesWithArguments;
    property SholdApplyFormating: Boolean read GetSholdApplyFormating;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    class function CreateField: TdxCalculatedFieldBase; virtual;
    function CanUseSwitchWithoutArgument(const AFieldSpecificSwitch: string): Boolean; virtual;
    function IsSwitchWithArgument(const AFieldSpecificSwitch: string): Boolean; virtual;
    procedure Initialize(APieceTable: TdxCustomPieceTable; ASwitches: TdxInstructionCollection); virtual;
    procedure BeforeCalculateFields(ASourcePieceTable: TdxCustomPieceTable;
      ADocumentField: TdxField); virtual;
    function Update(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode;
      ADocumentField: TdxField): TdxCalculatedFieldValue; override;
    procedure ApplyFieldFormatting(AValue: TdxCalculatedFieldValue;
      ACustomSeparators: TdxMailMergeCustomSeparators); virtual;
    procedure TryApplyFormatting(AValue: TdxCalculatedFieldValue;
      ACustomSeparators: TdxMailMergeCustomSeparators);
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
      AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; virtual; abstract;
    function GetInvariableSwitches: TArray<string>; virtual;
    function GetNativeSwithes: TArray<string>; virtual;
    function IsSwitchArgumentField(const AFieldSpecificSwitch: string): Boolean; virtual;

    property DateAndTimeFormatting: TdxNullableString read FDateAndTimeFormatting;
    property NumericFormatting: TdxNullableString read FNumericFormatting;
    property FrameworkStringFormat: TdxNullableString read FCommonStringFormat;
    property GeneralFormatting: TArray<string> read FGeneralFormatting;
    property Switches: TdxInstructionCollection read FSwitches;
    property CanPrepare: Boolean read GetCanPrepare;
    property AllowAsyncUpdate: Boolean read GetAllowAsyncUpdate;
  end;
  TdxCalculatedFieldBaseClass = class of TdxCalculatedFieldBase;

  { TdxSequentialFieldCustomManager }

  TdxSequentialFieldCustomManager = class abstract
  public
    function CalculateCounterValue(AField: TdxCalculatedFieldBase{TdxSequenceField}; ADocumentField: TdxField): Integer; virtual; abstract;
    procedure ClearCounters; virtual; abstract;
  end;

  { TdxFieldCollection }

  TdxFieldCollection = class(TdxFieldCollectionBase)
  strict private
    FSequentialFieldManager: TdxSequentialFieldCustomManager;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    destructor Destroy; override;

    procedure ClearCounters; override;

    property SequentialFieldManager: TdxSequentialFieldCustomManager read FSequentialFieldManager;
  end;

  { TdxFieldUpdater }

  TdxFieldUpdater = class
  strict private
    FPieceTable: TdxCustomPieceTable;
    function GetDocumentModel: TdxCustomDocumentModel;
    function GetFields: TdxFieldCollection;
  protected
    function GetMailMergeDataMode: TdxMailMergeDataMode; virtual;
    procedure UpdateInnerCodeFields(AParentField: TdxField; AUpdateType: TdxUpdateFieldOperationType);
    procedure UpdateInnerResultFields(AParentField: TdxField; AUpdateType: TdxUpdateFieldOperationType);

    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
    property PieceTable: TdxCustomPieceTable read FPieceTable;
    property Fields: TdxFieldCollection read GetFields;
  public
    constructor Create(APieceTable: TdxCustomPieceTable);
    procedure UpdateFields(AUpdateType: TdxUpdateFieldOperationType); overload;
    procedure UpdateFields(const AFields: TdxFieldList; AParent: TdxField; AUpdateType: TdxUpdateFieldOperationType); overload;
    procedure UpdateFields(const AFieldsToUpdate: TdxFieldList; AUpdateType: TdxUpdateFieldOperationType); overload;
    procedure UpdateFieldsAsync(const AFieldsToUpdate: TdxFieldList;
      AUpdateType: TdxUpdateFieldOperationType);
    procedure UpdateFieldsCore(const AFieldsToUpdate: TdxFieldList;
      AUpdateType: TdxUpdateFieldOperationType);
    function GetFieldsToUpdate(const AFields: TdxFieldList;
      AParent: TdxField): TdxFieldList; overload;
    function GetFieldsToUpdate(const AFields: TdxFieldCollection;
      AParent: TdxField): TdxFieldList; overload;
    procedure UpdateFieldAndNestedFields(AField: TdxField);
    procedure PrepareFieldsCore(const AFieldsToUpdate: TdxFieldList;
      AUpdateType: TdxUpdateFieldOperationType);
    procedure PrepareFieldUpdate(AField: TdxField;
      AUpdateType: TdxUpdateFieldOperationType); virtual;
    procedure PrepareFieldAndNestedFields(AField: TdxField; AUpdateType: TdxUpdateFieldOperationType);
    procedure PrepareNestedFieldsUpdate(AField: TdxField; AUpdateType: TdxUpdateFieldOperationType);
    function UpdateField(AField: TdxField; AMailMergeDataMode: TdxMailMergeDataMode): TdxUpdateFieldOperationResult; overload;
    function UpdateField(AField: TdxField; AMailMergeDataMode: TdxMailMergeDataMode;
      AUpdateType: TdxUpdateFieldOperationType): TdxUpdateFieldOperationResult; overload;
    procedure UpdateFieldRecursive(AField: TdxField; AUpdateType: TdxUpdateFieldOperationType);
    function GetInnerCodeFields(const AFields: TdxFieldList; AParentField: TdxField): TdxFieldList; virtual;
    function GetInnerResultFields(const AFields: TdxFieldList; AParentField: TdxField): TdxFieldList;
  end;

  { TdxMailMergeFieldUpdater }

  TdxMailMergeFieldUpdater = class(TdxFieldUpdater)
  protected
    function GetMailMergeDataMode: TdxMailMergeDataMode; override;
  end;

  { TdxCalculateFieldResult }

  TdxCalculateFieldResult = class(TcxIUnknownObject)
  strict private
    FValue: TdxCalculatedFieldValue;
    FUpdateType: TdxUpdateFieldOperationTypes;
    function GetOptions: TdxFieldResultOptions;
    function GetMergeFormat: Boolean;
    function GetCharFormat: Boolean;
    function GetMailMergeField: Boolean;
    function GetKeepOldResult: Boolean;
    function GetApplyFieldCodeFormatting: Boolean;
    function GetSuppressMergeUseFirstParagraphStyle: Boolean;
  public
    constructor Create(AValue: TdxCalculatedFieldValue; AUpdateType: TdxUpdateFieldOperationTypes);
    destructor Destroy; override;

    property Value: TdxCalculatedFieldValue read FValue;
    property Options: TdxFieldResultOptions read GetOptions;
    property UpdateType: TdxUpdateFieldOperationTypes read FUpdateType;
    property MergeFormat: Boolean read GetMergeFormat;
    property CharFormat: Boolean read GetCharFormat;
    property MailMergeField: Boolean read GetMailMergeField;
    property KeepOldResult: Boolean read GetKeepOldResult;
    property ApplyFieldCodeFormatting: Boolean read GetApplyFieldCodeFormatting;
    property SuppressMergeUseFirstParagraphStyle: Boolean read GetSuppressMergeUseFirstParagraphStyle;
  end;

  { TdxUpdateFieldOperationResult }

  TdxUpdateFieldOperationResult = class
  public
    class var
      DisabledUpdate: TdxUpdateFieldOperationResult;
  strict private
    FUpdateFieldResult: TdxUpdateFieldResult;
    FSuppressUpdateInnerCodeFields: Boolean;
    class constructor Initialize;
    class destructor Finalize;
  public
    constructor Create(AUpdateFieldResult: TdxUpdateFieldResult; ASuppressUpdateInnerCodeFields: Boolean);

    property UpdateFieldResult: TdxUpdateFieldResult read FUpdateFieldResult;
    property SuppressUpdateInnerCodeFields: Boolean read FSuppressUpdateInnerCodeFields;
  end;

  { TdxDocumentVariableCollection }

  TdxDocumentVariableCollection = class(TcxIUnknownObject, IdxRichEditDocumentVariableCollection)
  strict private
    FVariables: TDictionary<string, TValue>;
    FDocumentModel: TdxCustomDocumentModel;
    function GetCount: Integer;
    function GetItem(const AName: string): TValue;
    procedure SetItem(const AName: string; const Value: TValue);
    function IdxRichEditDocumentVariableCollection.GetVariableNames = GetNames;
    function GetNames: TArray<string>;
    function GetVariableValue(const AVariableName: string): TValue; overload;
  protected
    procedure SetVariableValue(const AVariableName: string; const AValue: TValue);
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel);
    destructor Destroy; override;

    procedure Add(const AName: string; const AValue: TValue);
    procedure Clear;
    function GetVariableNames: TDictionary<string, TValue>.TKeyCollection;
    function GetVariableValue(const AVariableName: string; AArguments: TdxArgumentCollection): TValue; overload;
    procedure Remove(const AName: string);

    property Count: Integer read GetCount;
    property Items[const AName: string]: TValue read GetItem write SetItem; default;
  end;

  { TdxFieldResultFormatting }

  TdxFieldResultFormatting = class
  private
    FGeneralFormatting: TArray<string>;
    FNumericFormatting: TdxNullableString;
  protected
    function GetRecalculateOnSecondaryFormatting: Boolean; virtual; abstract;
    function GetGeneralFormatting: TArray<string>; virtual;
    function GetValueCore(AFormatter: TObject; ADocumentModel: TdxCustomDocumentModel): Integer; virtual; abstract;
  public
    constructor Create(const ANumericFormatting: TdxNullableString; const AGeneralFormatting: TArray<string>);
    function Clone: TdxFieldResultFormatting; virtual;
    function ApplyImplicitFormatting(AFormatter: TObject; const AValue: string; AIntValue: Integer): string; virtual;
    function GetValue(AFormatter: TObject; ADocumentModel: TdxCustomDocumentModel): string;

    property NumericFormatting: TdxNullableString read FNumericFormatting;
    property GeneralFormatting: TArray<string> read GetGeneralFormatting;
    property RecalculateOnSecondaryFormatting: Boolean read GetRecalculateOnSecondaryFormatting;
  end;
  TdxFieldResultFormattingClass = class of TdxFieldResultFormatting;

implementation

uses
  Contnrs, Character, Variants,
  dxTypeHelpers, cxGeometry, cxVariants,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.CopyManager,
  dxRichEdit.DocumentModel.FieldFormatter,
  dxRichEdit.DocumentModel.Fields.PageRefField,
  dxRichEdit.DocumentModel.Fields.SequenceField,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Fields.CharactersGroupIterator,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.FieldCalculatorService,
  dxRichEdit.DocumentModel.NumericFieldFormatter,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.History.FieldHistory,
  dxRichEdit.InternalRichEditDocumentServer,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs;

type
  { TdxUpdateFieldOperation }

  TdxUpdateFieldOperation = class
  strict private
    FPieceTable: TdxPieceTable;
    FMailMergeDataMode: TdxMailMergeDataMode;
    FField: TdxField;
    FStartResultPos: TdxDocumentModelPosition;
    FEndResultLogPos: TdxDocumentLogPosition;
    function GetBookmarksByEndPosition(APieceTable: TdxPieceTable; APosition: TdxDocumentLogPosition): TdxBookmarkList;
    procedure ModifyBookmarks(APieceTable: TdxPieceTable; ABookmarks: TdxBookmarkList; ANewEndLogPosition: TdxDocumentLogPosition);
    function GetDocumentModel: TdxDocumentModel;
  protected
    procedure BeginFieldUpdate(AField: TdxField); virtual;
    procedure EndFieldUpdate; virtual;
    function ReplaceFieldResult(AResult: TdxCalculateFieldResult;
      AUpdateType: TdxUpdateFieldOperationType): TdxUpdateFieldOperationResult; virtual;
    function IsFieldSelected(AStartPosition: TdxDocumentLogPosition;
      AEndPosition: TdxDocumentLogPosition): Boolean;
    function MinLogPosition(APos1: TdxDocumentLogPosition; APos2: TdxDocumentLogPosition): TdxDocumentLogPosition;
    function MaxLogPosition(APos1: TdxDocumentLogPosition; APos2: TdxDocumentLogPosition): TdxDocumentLogPosition;
    procedure SetInlinePictureFieldSize(const ASize: TSize);
    function GetInlinePictureFieldSize: TSize; virtual;
    procedure FixLastParagraphFormatting(ATargetModel: TdxDocumentModel;
      const AResultStart: TdxDocumentModelPosition); virtual;
    function DeleteOldFieldResult(const AStartPosition, AEndPosition: TdxDocumentModelPosition; ASuppressMergeUseFirstParagraphStyle: Boolean): TdxDocumentModelPosition; virtual;
    procedure RemoveField; virtual;
    function GetDeleteStart(ADeleteFieldCode: Boolean): TdxDocumentModelPosition; virtual;
    function GetDeleteEnd(ADeleteFieldCode: Boolean): TdxDocumentModelPosition; virtual;
    procedure CopyCharacterFormatToDocumentModel(ATargetModel: TdxDocumentModel); virtual;
    procedure CopyFormattingFromCodeFirstChar(const ATargetPos: TdxDocumentModelPosition); overload; virtual;
    procedure CopyFormattingFromCodeFirstChar(APieceTable: TdxPieceTable;
      ASourceRun: TdxTextRunBase; ATargetPos: TdxDocumentLogPosition; ALength: Integer); overload; virtual;
    function GetSourceRun: TdxTextRunBase; virtual;
    function GetCharacter(const APos: TdxDocumentModelPosition): Char;
    procedure CopyParagraphFormatToDocumentModel(ATargetModel: TdxDocumentModel); virtual;
    procedure TrySplitRun(const ATo: TdxDocumentModelPosition); virtual;
    procedure CopyCharacterFormatToRuns(ASourceRun: TdxTextRunBase; const ATargetPos: TdxDocumentModelPosition);
    procedure CopyCharacterFormatCore(ASourceRun: TdxTextRunBase; ATargetRun: TdxTextRunBase);
    procedure CopyParagraphFormatCore(ASourceParagraph: TdxParagraph; ATargetParagraph: TdxParagraph);
    function GetTextRun(const APos: TdxDocumentModelPosition): TdxTextRunBase;
    function GetParagraph(const APos: TdxDocumentModelPosition): TdxParagraph;
    function GetNextCharacterGroupPosition(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function GetNextParagraphPosition(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
  public
    constructor Create(APieceTable: TdxPieceTable; AMailMergeDataMode: TdxMailMergeDataMode);
    function Execute(AField: TdxField; AUpdateType: TdxUpdateFieldOperationType): TdxUpdateFieldOperationResult; virtual;
    function InsertResult(AField: TdxField; AResult: TdxCalculateFieldResult;
      AUpdateType: TdxUpdateFieldOperationType): TdxUpdateFieldOperationResult;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read FPieceTable;
    property MailMergeDataMode: TdxMailMergeDataMode read FMailMergeDataMode;
  end;

{ TdxDocumentLogInterval }

constructor TdxDocumentLogInterval.Create(AStart: TdxDocumentLogPosition; ALength: Integer);
begin
  FStart := AStart;
  FLength := ALength;
end;

class function TdxDocumentLogInterval.Null: TdxDocumentLogInterval;
begin
  Result := TdxDocumentLogInterval.Create(-1, 0);
end;

function TdxDocumentLogInterval.IsNull: Boolean;
begin
  Result := Length = 0;
end;

function TdxDocumentLogInterval.IsValid: Boolean;
begin
  Result := (Start >= 0) and (Length >= 0);
end;

{ TdxInstructionCollection }

constructor TdxInstructionCollection.Create;
begin
  inherited Create;
  FSwitches := TdxNamedObjectDictionary<TdxTokenList>.Create(True);
  FSwitchIntervals := TdxNamedObjectDictionary<TdxDocumentLogIntervalList>.Create(True);
  FArguments := TdxTokenList.Create;
end;

destructor TdxInstructionCollection.Destroy;
begin
  FreeAndNil(FSwitches);
  FreeAndNil(FSwitchIntervals);
  FreeAndNil(FArguments);
  inherited Destroy;
end;

procedure TdxInstructionCollection.AddSwitch(const AFieldSwitch: string; const AInterval: TdxDocumentLogInterval);
begin
  AddSwitch(AFieldSwitch, AInterval, nil);
end;

procedure TdxInstructionCollection.AddSwitch(const AFieldSwitch: string;
  const AInterval: TdxDocumentLogInterval; AFieldArgument: IdxToken);
var
  ATokens: TdxTokenList;
  AIntervals: TdxDocumentLogIntervalList;
begin
  if not FSwitches.TryGetValue(AFieldSwitch, ATokens) then
  begin
    ATokens := TdxTokenList.Create;
    ATokens.Add(AFieldArgument);
    FSwitches.Add(AFieldSwitch, ATokens);
    AIntervals := TdxDocumentLogIntervalList.Create;
    AIntervals.Add(AInterval);
    FSwitchIntervals.Add(AFieldSwitch, AIntervals);
  end
  else
  begin
    ATokens.Add(AFieldArgument);
    FSwitchIntervals[AFieldSwitch].Add(AInterval);
  end;
end;

procedure TdxInstructionCollection.AddArgument(const AFieldArgument: IdxToken);
begin
  FArguments.Add(AFieldArgument);
end;

function TdxInstructionCollection.GetInt(const AFieldSwitch: string): Integer;
var
  AResult: TdxTokenList;
  AToken: IdxToken;
begin
  if FSwitches.TryGetValue('\' + AFieldSwitch, AResult) then
  begin
    if AResult = nil then
      Exit(0);
    AToken := GetDefaultToken(AResult);
    if AToken = nil then
      Exit(0);
    Result := StrToInt(AToken.Value);
  end
  else
    Result := 0;
end;

function TdxInstructionCollection.GetString(const AFieldSwitch: string): TdxNullableString;
var
  AResult: TdxTokenList;
  AToken: IdxToken;
begin
  if FSwitches.TryGetValue('\' + AFieldSwitch, AResult) then
  begin
    if AResult = nil then
      Exit(TdxNullableString.Null);
    AToken := GetDefaultToken(AResult);
    if AToken = nil then
      Exit(TdxNullableString.Null);
    Exit(AToken.Value);
  end
  else
    Exit(TdxNullableString.Null);
end;

function TdxInstructionCollection.GetStringArray(const AFieldSwitch: string): TArray<string>;
var
  AResult: TdxTokenList;
  ACount, I: Integer;
  AArray: TdxStringList;
begin
  SetLength(Result, 0);
  if FSwitches.TryGetValue('\' + AFieldSwitch, AResult) then
  begin
    if AResult = nil then
      Exit;
    ACount := AResult.Count;
    AArray := TdxStringList.Create;
    try
      for I := 0 to ACount - 1 do
        if AResult[I] <> nil then
          AArray.Add(AResult[I].Value);
      Exit(AArray.ToArray);
    finally
      AArray.Free;
    end;
  end;
end;

function TdxInstructionCollection.GetBool(const AFieldSwitch: string): Boolean;
begin
  Result := FSwitches.ContainsKey('\' + AFieldSwitch);
end;

function TdxInstructionCollection.GetNullableInt(const AFieldSwitch: string): TdxNullableInteger;
var
  AResult: TdxTokenList;
  AToken: IdxToken;
begin
  Result.Reset;
  if FSwitches.TryGetValue('\' + AFieldSwitch, AResult) then
  begin
    if AResult = nil then
      Exit;
    AToken := GetDefaultToken(AResult);
    if AToken = nil then
      Exit;
    Result := StrToInt(AToken.Value);
    Exit;
  end
  else
    Exit;
end;

function TdxInstructionCollection.GetDefaultToken(const ATokens: TdxTokenList): IdxToken;
begin
  Result := ATokens.Last;
end;

function TdxInstructionCollection.GetDefaultInterval(AIntervals: TdxDocumentLogIntervalList): TdxDocumentLogInterval;
begin
  Result := AIntervals[AIntervals.Count - 1];
end;

function TdxInstructionCollection.GetArgument(AIndex: Integer): IdxToken;
begin
  Result := FArguments[AIndex];
end;

function TdxInstructionCollection.GetArgumentAsString(AIndex: Integer): string;
var
  AToken: IdxToken;
begin
  if AIndex >= FArguments.Count then
    Exit('');
  AToken := GetArgument(AIndex);
  Result := AToken.Value;
end;

function TdxInstructionCollection.GetArgumentAsDocumentInterval(AIndex: Integer): TdxDocumentLogInterval;
var
  AToken: IdxToken;
begin
  if AIndex >= FArguments.Count then
    Exit(TdxDocumentLogInterval.Null);
  AToken := GetArgument(AIndex);
  Result := TdxDocumentLogInterval.Create(AToken.Position, AToken.Length);
end;

function TdxInstructionCollection.GetSwitchArgumentDocumentInterval(const AFieldSwitch: string): TdxDocumentLogInterval;
begin
  Result := GetSwitchArgumentDocumentInterval(AFieldSwitch, False);
end;

function TdxInstructionCollection.GetSwitchArgumentDocumentInterval(const AFieldSwitch: string;
  AIncludeFullInterval: Boolean): TdxDocumentLogInterval;
var
  AResult: TdxTokenList;
  AToken: IdxToken;
begin
  Result := TdxDocumentLogInterval.Create(0, 0);
  if not FSwitches.TryGetValue('\' + AFieldSwitch, AResult) then
    Exit;
  if AResult = nil then
    Exit;
  AToken := GetDefaultToken(AResult);
  if AToken = nil then
    Exit;
  if AIncludeFullInterval then
  begin
    if AToken.ActualKind = TdxTokenKind.QuotedText then
      Exit(TdxDocumentLogInterval.Create(AToken.Position - 1, AToken.Length + 2))
    else
      if AToken.ActualKind = TdxTokenKind.Template then
        Exit(TdxDocumentLogInterval.Create(AToken.Position - 1, AToken.Length + 3));
  end;
  Result := TdxDocumentLogInterval.Create(AToken.Position, AToken.Length);
end;

function TdxInstructionCollection.GetSwitchDocumentInterval(const AFieldSwitch: string): TdxDocumentLogInterval;
var
  AResult: TdxDocumentLogIntervalList;
begin
  Result := TdxDocumentLogInterval.Create(0, 0);
  if FSwitchIntervals.TryGetValue('\' + AFieldSwitch, AResult) then
    Result := GetDefaultInterval(AResult);
end;

{ TdxCalculatedFieldBase }

constructor TdxCalculatedFieldBase.Create;
begin
  inherited Create;
end;

destructor TdxCalculatedFieldBase.Destroy;
begin
  FreeAndNil(FSwitches);
  inherited Destroy;
end;

class function TdxCalculatedFieldBase.CreateField: TdxCalculatedFieldBase;
begin
  dxAbstractError;
  Result := nil;
end;

function TdxCalculatedFieldBase.CanUseSwitchWithoutArgument(const AFieldSpecificSwitch: string): Boolean;
begin
  Result := False;
end;

function TdxCalculatedFieldBase.GetCanPrepare: Boolean;
begin
  Result := False;
end;

function TdxCalculatedFieldBase.GetAllowAsyncUpdate: Boolean;
begin
  Result := True;
end;

function TdxCalculatedFieldBase.MailMergeType: TdxFieldMailMergeType;
begin
  Result := TdxFieldMailMergeType.NonMailMerge;
end;

class function TdxCalculatedFieldBase.GetDefaultMailMergeFieldText(const AFieldName: string): string;
begin
  Result := Format('<<%s>>', [AFieldName]);
end;

class function TdxCalculatedFieldBase.CreateSwitchesWithArgument(
  const ASwitches: TArray<string>): TdxStringBooleanDictionary;
var
  ACount, I: Integer;
begin
  Result := CreateSwitchesWithArgument;
  ACount := Length(ASwitches);
  for I := 0 to ACount - 1 do
    Result.Add(Format('\%s', [ASwitches[I]]), True);
end;

class function TdxCalculatedFieldBase.CreateSwitchesWithArgument: TdxStringBooleanDictionary;
begin
  Result := TdxStringBooleanDictionary.Create;
end;

function TdxCalculatedFieldBase.IsSwitchWithArgument(const AFieldSpecificSwitch: string): Boolean;
begin
  Result := SwitchesWithArguments.ContainsKey(AFieldSpecificSwitch);
end;

procedure TdxCalculatedFieldBase.Initialize(APieceTable: TdxCustomPieceTable; ASwitches: TdxInstructionCollection);
begin
  FSwitches := ASwitches;
  FDateAndTimeFormatting := ASwitches.GetString('@');
  FNumericFormatting := ASwitches.GetString('#');
  FGeneralFormatting := ASwitches.GetStringArray('*');
  if TdxPieceTable(APieceTable).SupportFieldCommonStringFormat then
    FCommonStringFormat := ASwitches.GetString(FrameworkStringFormatSwitch);
  FUseCurrentCultureDateTimeFormat := TdxPieceTable(APieceTable).DocumentModel.FieldOptions.UseCurrentCultureDateTimeFormat;
end;

function TdxCalculatedFieldBase.InsertDefaultMailMergeText(ASourcePieceTable: TdxCustomPieceTable): Boolean;
var
  AFieldDataService: IdxFieldDataService;
begin
  if Ord(MailMergeType) and Ord(TdxFieldMailMergeType.NonMailMerge) <> 0 then
    Exit(False);
  AFieldDataService := TdxPieceTable(ASourcePieceTable).DocumentModel.GetService<IdxFieldDataService>;
  Result := (AFieldDataService = nil) or (not AFieldDataService.BoundMode);
end;

function TdxCalculatedFieldBase.Update(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  AValue: TdxCalculatedFieldValue;
  ACharacterFormatFlag: TdxFieldResultOptions;
begin
  Result := nil;
  if InsertDefaultMailMergeText(ASourcePieceTable) then
    Exit(TdxCalculatedFieldValue.Create(GetDefaultMailMergeFieldText(FieldTypeName), GetCharacterFormatFlag));
  AValue := GetCalculatedValueCore(ASourcePieceTable, AMailMergeDataMode, ADocumentField);
  try
    if AValue = TdxCalculatedFieldValue.InvalidValue then
      Exit(TdxCalculatedFieldValue.Create(GetDefaultMailMergeFieldText(FieldTypeName), GetCharacterFormatFlag));
    ApplyFieldFormatting(AValue, TdxPieceTable(ASourcePieceTable).DocumentModel.MailMergeOptions.CustomSeparators);
    ACharacterFormatFlag := GetCharacterFormatFlag;
    if Ord(MailMergeType) and Ord(TdxFieldMailMergeType.MailMerge) <> 0 then
      ACharacterFormatFlag := ACharacterFormatFlag + [TdxFieldResultOption.MailMergeField];
    Result := AValue.AddOptions(ACharacterFormatFlag);
  finally
    if AValue <> Result then
      AValue.Free;
  end;
end;

function TdxCalculatedFieldBase.GetCharacterFormatFlag: TdxFieldResultOptions;
var
  ALastIndex, I: Integer;
  AValue: string;
begin
  Result := [];
  if FGeneralFormatting = nil then
    Exit;
  ALastIndex := Length(FGeneralFormatting) - 1;
  for I := ALastIndex downto 0 do
  begin
  {$IFDEF DELPHIXE4}
    AValue := FGeneralFormatting[I].ToUpper;
  {$ELSE}
    AValue := TCharacter.ToUpper(FGeneralFormatting[I]);
  {$ENDIF}
    if (AValue = MergeFormatKeyword) or (AValue = MergeFormatInetKeyword) then
      Exit([TdxFieldResultOption.MergeFormat]);
    if AValue = CharFormatKeyword then
      Exit([TdxFieldResultOption.CharFormat]);
  end;
end;

function TdxCalculatedFieldBase.GetSholdApplyFormating: Boolean;
begin
  Result := True;
end;

procedure TdxCalculatedFieldBase.BeforeCalculateFields(ASourcePieceTable: TdxCustomPieceTable;
  ADocumentField: TdxField);
begin
//do nothing
end;

function TdxCalculatedFieldBase.TryApplyCommonStringFormat(AValue: TdxCalculatedFieldValue): Boolean;
var
  AHasCommonStringFormat, AShouldApplyFormatting: Boolean;
begin
  Result := False;
  AHasCommonStringFormat := not FCommonStringFormat.IsNull;
  AShouldApplyFormatting := AHasCommonStringFormat and not AValue.RawValue.IsEmpty;
  if AShouldApplyFormatting then
  begin
    Result := True;
  end;
end;

procedure TdxCalculatedFieldBase.ApplyFieldFormatting(AValue: TdxCalculatedFieldValue;
  ACustomSeparators: TdxMailMergeCustomSeparators);
var
  ACount, I: Integer;
  AGeneralFieldFormatter: TdxGeneralFieldFormatter;
  AGeneralFormattingSwitch: string;
  AGeneralFormattingSwitchUpperCase: string;
  AActualValue: string;
begin
  TryApplyFormatting(AValue, ACustomSeparators);
  if Length(GeneralFormatting) > 0 then
  begin
    ACount := Length(GeneralFormatting);
    AGeneralFieldFormatter := TdxGeneralFieldFormatter.Create;
    try
      for I := 0 to ACount - 1 do
      begin
        AGeneralFormattingSwitch := GeneralFormatting[I];
      {$IFDEF DELPHIXE4}
        AGeneralFormattingSwitchUpperCase := AGeneralFormattingSwitch.ToUpper;
      {$ELSE}
        AGeneralFormattingSwitchUpperCase := TCharacter.ToUpper(AGeneralFormattingSwitch);
      {$ENDIF}
        if (AGeneralFormattingSwitchUpperCase = MergeFormatKeyword) or
            (AGeneralFormattingSwitchUpperCase = MergeFormatInetKeyword) or
            (AGeneralFormattingSwitchUpperCase = CharFormatKeyword) then
          Continue;
        if not AValue.ConvertedToString and AValue.IsDocumentModelValue then
        begin
          if AGeneralFieldFormatter.IsGeneralDocumentModelStringFormatter(AGeneralFormattingSwitch) then
            AGeneralFieldFormatter.FormatPieceTable(TdxDocumentModel(AValue.DocumentModel).MainPieceTable, AGeneralFormattingSwitch)
          else
          begin
            AActualValue := GetDocumentModelStringValue(AValue.DocumentModel);
            AActualValue := AGeneralFieldFormatter.Format(AActualValue, AActualValue, AGeneralFormattingSwitch);
            AValue.SetValueConvertedToString(AActualValue);
          end;
        end
        else
        begin
          if not AValue.ConvertedToString and not AValue.IsDocumentModelValue then
            AValue.SetValueConvertedToString(VarToStr(AValue.RawValue.AsVariant));
          AValue.SetValueConvertedToString(AGeneralFieldFormatter.Format(AValue.Text, AValue.Text, AGeneralFormattingSwitch));
        end;
      end;
    finally
      AGeneralFieldFormatter.Free;
    end;
  end;
  if not AValue.IsDocumentModelValue and not AValue.ConvertedToString then
  begin
    if not AValue.RawValue.IsEmpty then
      AValue.SetValueConvertedToString(AValue.RawValue.ToString)
    else
      AValue.SetValueConvertedToString('');
  end;
end;

procedure TdxCalculatedFieldBase.TryApplyFormatting(AValue: TdxCalculatedFieldValue; ACustomSeparators: TdxMailMergeCustomSeparators);
begin
  if not SholdApplyFormating then
    Exit;
  if TryApplyCommonStringFormat(AValue) then
    Exit;
  if TryApplyDateAndTimeFormatting(AValue) then
    Exit;
  TryApplyNumericFormatting(AValue, ACustomSeparators);
end;

function TdxCalculatedFieldBase.GetObjectForFormat(AValue: TdxCalculatedFieldValue; const ACommonStringFormat: string): TValue;
var
  ADateTime: TDateTime;
  ANumber: Double;
begin
  if TryConvertToDateTime(AValue, ADateTime) then
    Exit(ADateTime);
  if TryConvertToDouble(AValue, ANumber) then
    Exit(ANumber);
  if AValue.IsDocumentModelValue then
    Exit(GetDocumentModelStringValue(AValue.DocumentModel))
  else
    Exit(AValue.RawValue);
end;

function TdxCalculatedFieldBase.TryApplyDateAndTimeFormatting(AValue: TdxCalculatedFieldValue): Boolean;
var
  AHasDateAndTimeFormatting, AShouldApplyFormatting: Boolean;
  ADateTime: TDateTime;
  ADateTimeFieldFormatter: TdxDateTimeFieldFormatter;
begin
  Result := False;
  AHasDateAndTimeFormatting := not FDateAndTimeFormatting.IsNull;
  AShouldApplyFormatting := AHasDateAndTimeFormatting or AValue.IsDateTimeValue;
  if not AShouldApplyFormatting then
    Exit;
  if not TryConvertToDateTime(AValue, ADateTime) then
    Exit;
  ADateTimeFieldFormatter := CreateDateTimeFieldFormatter;
  try
    if AHasDateAndTimeFormatting then
      AValue.SetValueConvertedToString(ADateTimeFieldFormatter.Format(ADateTime, DateAndTimeFormatting.Value, AHasDateAndTimeFormatting))
    else
      AValue.SetValueConvertedToString(ADateTimeFieldFormatter.Format(ADateTime, '', AHasDateAndTimeFormatting));
  finally
    ADateTimeFieldFormatter.Free;
  end;
  Result := True;
end;

function TdxCalculatedFieldBase.CreateDateTimeFieldFormatter: TdxDateTimeFieldFormatter;
begin
  Result := TdxDateTimeFieldFormatter.Create;
  Result.UseCurrentCultureDateTimeFormat := UseCurrentCultureDateTimeFormat;
end;

function TdxCalculatedFieldBase.TryApplyNumericFormatting(AValue: TdxCalculatedFieldValue; ACustomSeparators: TdxMailMergeCustomSeparators): Boolean;
var
  AHasNumericFormatting: Boolean;
  ADoubleVal: Double;
  ANumericFieldFormatter: TdxNumericFieldFormatter;
begin
  AHasNumericFormatting := not FNumericFormatting.IsNull;
  if not AHasNumericFormatting then
    Exit(False);
  if not TryConvertToDouble(AValue, ADoubleVal) then
    Exit(False);
  ANumericFieldFormatter := TdxNumericFieldFormatter.Create;
  try
    ANumericFieldFormatter.CustomSeparators.Assign(ACustomSeparators);
    if AHasNumericFormatting then
      AValue.SetValueConvertedToString(ANumericFieldFormatter.Format(ADoubleVal, NumericFormatting.Value, AHasNumericFormatting))
    else
      AValue.SetValueConvertedToString(ANumericFieldFormatter.Format(ADoubleVal, '', AHasNumericFormatting));
    Result := True;
  finally
    ANumericFieldFormatter.Free;
  end;
end;

function TdxCalculatedFieldBase.TryConvertToDateTime(AValue: TdxCalculatedFieldValue; out AResult: TDateTime): Boolean;
var
  ARawValue: TValue;
begin
  ARawValue := AValue.RawValue;
  if AValue.IsDocumentModelValue then
    ARawValue := GetDocumentModelStringValue(AValue.DocumentModel);
  Result := not ARawValue.IsEmpty and ARawValue.IsDateTime;
  if not Result then
    Exit;
  try
    AResult := ARawValue.AsType<TDateTime>;
    Exit(True);
  except
    AResult := MinDateTime;
    Result := False;
  end;
end;

function TdxCalculatedFieldBase.TryConvertToDouble(AValue: TdxCalculatedFieldValue; out AResult: Double): Boolean;
var
  ARawValue: TValue;
  V: Variant;
begin
  ARawValue := AValue.RawValue;
  if AValue.IsDocumentModelValue then
    ARawValue := GetDocumentModelStringValue(AValue.DocumentModel);
  Result := True;
  try
    if not ARawValue.IsEmpty and ARawValue.IsString and not IsEmptyString(ARawValue) then
      Result := TryStrToFloat(ARawValue.AsString, AResult)
    else
      if ARawValue.IsExtended then
        AResult := ARawValue.AsExtended
      else
      begin
        V := ARawValue.AsVariant;
        Result := VarIsNumeric(V);
        if Result then
          AResult := V;
      end;
  except
    AResult := 0;
    Result := False;
  end;
end;

function TdxCalculatedFieldBase.IsEmptyString(AValue: TValue): Boolean;
begin
  Result := AValue.IsEmpty or (AValue.IsString and (Length(AValue.AsString) = 0));
end;

function TdxCalculatedFieldBase.GetDocumentModelStringValue(ADocumentModel: TdxCustomDocumentModel): string;
begin
  Result := TdxDocumentModel(ADocumentModel).InternalAPI.GetDocumentPlainTextContent;
end;

function TdxCalculatedFieldBase.GetNativeSwithes: TArray<string>;
begin
  SetLength(Result, 0);
end;

function TdxCalculatedFieldBase.GetInvariableSwitches: TArray<string>;
begin
  SetLength(Result, 0);
end;

function TdxCalculatedFieldBase.IsSwitchArgumentField(const AFieldSpecificSwitch: string): Boolean;
begin
  Result := False;
end;

{ TdxFieldCollection }

constructor TdxFieldCollection.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FSequentialFieldManager := TdxSequentialFieldManager.Create(TdxPieceTable(APieceTable));
end;

destructor TdxFieldCollection.Destroy;
begin
  FreeAndNil(FSequentialFieldManager);
  inherited Destroy;
end;

procedure TdxFieldCollection.ClearCounters;
begin
  FSequentialFieldManager.ClearCounters;
end;

{ TdxFieldUpdater }

constructor TdxFieldUpdater.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
end;

function TdxFieldUpdater.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := TdxPieceTable(FPieceTable).DocumentModel;
end;

function TdxFieldUpdater.GetFields: TdxFieldCollection;
begin
  Result := TdxPieceTable(FPieceTable).Fields;
end;

procedure TdxFieldUpdater.UpdateFields(AUpdateType: TdxUpdateFieldOperationType);
begin
  UpdateFields(Fields.InnerList, nil, AUpdateType);
end;

procedure TdxFieldUpdater.UpdateFields(const AFields: TdxFieldList; AParent: TdxField; AUpdateType: TdxUpdateFieldOperationType);
var
  AList: TdxFieldList;
begin
  AList := GetFieldsToUpdate(AFields, AParent);
  try
    UpdateFields(AList, AUpdateType);
  finally
    AList.Free;
  end;
end;

procedure TdxFieldUpdater.UpdateFields(const AFieldsToUpdate: TdxFieldList;
  AUpdateType: TdxUpdateFieldOperationType);
var
  ADocumentModel: TdxCustomDocumentModel;
begin
  ADocumentModel := DocumentModel;
  ADocumentModel.BeginUpdate;
  try
    PrepareFieldsCore(AFieldsToUpdate, AUpdateType);
    UpdateFieldsCore(AFieldsToUpdate, AUpdateType);
  finally
    ADocumentModel.EndUpdate;
  end;
end;

procedure TdxFieldUpdater.UpdateFieldsAsync(const AFieldsToUpdate: TdxFieldList;
  AUpdateType: TdxUpdateFieldOperationType);
begin
end;

procedure TdxFieldUpdater.UpdateFieldsCore(const AFieldsToUpdate: TdxFieldList;
  AUpdateType: TdxUpdateFieldOperationType);
var
  ACount, I: Integer;
begin
  ACount := AFieldsToUpdate.Count;
  for I := 0 to ACount - 1 do
    UpdateFieldRecursive(AFieldsToUpdate[I], AUpdateType);
end;

function TdxFieldUpdater.GetFieldsToUpdate(const AFields: TdxFieldCollection;
  AParent: TdxField): TdxFieldList;
var
  ACount, I: Integer;
begin
  Result := TdxFieldList.Create;
  ACount := AFields.Count;
  for I := 0 to ACount - 1 do
    if AFields[I].Parent = AParent then
      Result.Add(AFields[I]);
end;

function TdxFieldUpdater.GetFieldsToUpdate(const AFields: TdxFieldList; AParent: TdxField): TdxFieldList;
var
  ACount, I: Integer;
begin
  Result := TdxFieldList.Create;
  Result.Capacity := AFields.Count;
  ACount := AFields.Count;
  for I := 0 to ACount - 1 do
    if AFields[I].Parent = AParent then
      Result.Add(AFields[I]);
end;

procedure TdxFieldUpdater.UpdateFieldAndNestedFields(AField: TdxField);
var
  ADocumentModel: TdxCustomDocumentModel;
begin
  ADocumentModel := DocumentModel;
  ADocumentModel.BeginUpdate;
  try
    PrepareFieldAndNestedFields(AField, TdxUpdateFieldOperationType.Normal);
    UpdateFieldRecursive(AField, TdxUpdateFieldOperationType.Normal);
  finally
    ADocumentModel.EndUpdate;
  end;
end;

procedure TdxFieldUpdater.PrepareFieldsCore(const AFieldsToUpdate: TdxFieldList; AUpdateType: TdxUpdateFieldOperationType);
var
  ACount, I: Integer;
begin
  ACount := AFieldsToUpdate.Count;
  for I := 0 to ACount - 1 do
    PrepareFieldAndNestedFields(AFieldsToUpdate[I], AUpdateType);
end;

procedure TdxFieldUpdater.PrepareFieldUpdate(AField: TdxField; AUpdateType: TdxUpdateFieldOperationType);
var
  AFieldCalculatorService: IdxFieldCalculatorService;
begin
  AFieldCalculatorService := DocumentModel.GetService<IdxFieldCalculatorService>;
  if AFieldCalculatorService <> nil then
    AFieldCalculatorService.PrepareField(PieceTable, AField, AUpdateType);
end;

procedure TdxFieldUpdater.PrepareFieldAndNestedFields(AField: TdxField; AUpdateType: TdxUpdateFieldOperationType);
begin
  PrepareNestedFieldsUpdate(AField, AUpdateType);
  PrepareFieldUpdate(AField, AUpdateType);
end;

procedure TdxFieldUpdater.PrepareNestedFieldsUpdate(AField: TdxField; AUpdateType: TdxUpdateFieldOperationType);
var
  AFields: TdxFieldCollection;
  AComparator: TdxFieldRunIndexComparable;
  AIndex: Integer;
  ACurrentField: TdxField;
begin
  AFields := Fields;
  AComparator := TdxFieldRunIndexComparable.Create(AField.FirstRunIndex);
  try
    if not TdxAlgorithms1<TdxField>.BinarySearch(AFields.InnerList, AComparator, AIndex) then
    begin
      if AIndex >= AFields.Count then
        TdxRichEditExceptions.ThrowInternalException;
    end;
  finally
    AComparator.Free;
  end;
  ACurrentField := AFields[AIndex];
  while ACurrentField <> AField do
  begin
    PrepareFieldUpdate(ACurrentField, AUpdateType);
    Inc(AIndex);
    ACurrentField := AFields[AIndex];
  end;
end;

function TdxFieldUpdater.UpdateField(AField: TdxField; AMailMergeDataMode: TdxMailMergeDataMode): TdxUpdateFieldOperationResult;
begin
  Result := UpdateField(AField, AMailMergeDataMode, TdxUpdateFieldOperationType.Normal);
end;

function TdxFieldUpdater.UpdateField(AField: TdxField; AMailMergeDataMode: TdxMailMergeDataMode; AUpdateType: TdxUpdateFieldOperationType): TdxUpdateFieldOperationResult;
var
  AOperation: TdxUpdateFieldOperation;
  ADocumentModel: TdxCustomDocumentModel;
begin
  if AField.DisableUpdate then
    Exit(TdxUpdateFieldOperationResult.DisabledUpdate);
  ADocumentModel := DocumentModel;
  ADocumentModel.BeginUpdate;
  try
    AOperation := TdxUpdateFieldOperation.Create(TdxPieceTable(PieceTable), AMailMergeDataMode);
    try
      Exit(AOperation.Execute(AField, AUpdateType));
    finally
      AOperation.Free;
    end;
  finally
    ADocumentModel.EndUpdate;
  end;
end;

procedure TdxFieldUpdater.UpdateFieldRecursive(AField: TdxField; AUpdateType: TdxUpdateFieldOperationType);
var
  AMode: TdxMailMergeDataMode;
  AResult: TdxUpdateFieldOperationResult;
begin
  if AField.DisableUpdate then
    Exit;
  AMode := GetMailMergeDataMode;
  UpdateInnerCodeFields(AField, AUpdateType);
  AResult := UpdateField(AField, AMode, AUpdateType);
  try
    if AResult.UpdateFieldResult <> TdxUpdateFieldResult.FieldUpdatedAndCodeDeleted then
    begin
      if not AResult.SuppressUpdateInnerCodeFields then
        UpdateInnerResultFields(AField, AUpdateType);
    end;
  finally
    if AResult <> TdxUpdateFieldOperationResult.DisabledUpdate then
      AResult.Free;
  end;
end;

function TdxFieldUpdater.GetMailMergeDataMode: TdxMailMergeDataMode;
begin
  Result := TdxDocumentModel(DocumentModel).GetMailMergeDataMode;
end;

procedure TdxFieldUpdater.UpdateInnerCodeFields(AParentField: TdxField; AUpdateType: TdxUpdateFieldOperationType);
var
  AInnerCodeFields: TdxFieldList;
  ACount, I: Integer;
begin
  AInnerCodeFields := GetInnerCodeFields(Fields.InnerList, AParentField);
  try
    ACount := AInnerCodeFields.Count;
    for I := 0 to ACount - 1 do
      UpdateFieldRecursive(AInnerCodeFields[I], AUpdateType);
  finally
    AInnerCodeFields.Free;
  end;
end;

procedure TdxFieldUpdater.UpdateInnerResultFields(AParentField: TdxField; AUpdateType: TdxUpdateFieldOperationType);
var
  AInnerResultFields: TdxFieldList;
  ACount, I: Integer;
  AService: TdxFieldCalculatorService;
  AFieldBase: TdxCalculatedFieldBase;
begin
  AInnerResultFields := GetInnerResultFields(Fields.InnerList, AParentField);
  try
    ACount := AInnerResultFields.Count;
    for I := 0 to ACount - 1 do
    begin
      AService := TdxFieldCalculatorService.Create;
      try
        AFieldBase := AService.ParseField(TdxPieceTable(PieceTable), AInnerResultFields[I]);
        try
          if not (AFieldBase is TdxPageRefField) and (AFieldBase <> nil) then
            UpdateFieldRecursive(AInnerResultFields[I], AUpdateType);
        finally
          AFieldBase.Free;
        end;
      finally
        AService.Free;
      end;
    end;
  finally
    AInnerResultFields.Free;
  end;
end;

function TdxFieldUpdater.GetInnerCodeFields(const AFields: TdxFieldList;
  AParentField: TdxField): TdxFieldList;
var
  AFieldsCount, I: Integer;
begin
  AFieldsCount := AFields.Count;
  Result := TdxFieldList.Create;
  for I := 0 to AFieldsCount - 1 do
    if (AFields[I].Parent = AParentField) and (AFields[I].&Result.&End < AParentField.Code.&End) then
      Result.Add(AFields[I]);
end;

function TdxFieldUpdater.GetInnerResultFields(const AFields: TdxFieldList; AParentField: TdxField): TdxFieldList;
var
  AFieldsCount, I: Integer;
begin
  AFieldsCount := AFields.Count;
  Result := TdxFieldList.Create;
  for I := 0 to AFieldsCount - 1 do
    if (AFields[I].Parent = AParentField) and (AFields[I].Code.Start >= AParentField.&Result.Start) then
      Result.Add(AFields[I]);
end;

{ TdxMailMergeFieldUpdater }

function TdxMailMergeFieldUpdater.GetMailMergeDataMode: TdxMailMergeDataMode;
begin
  Result := TdxMailMergeDataMode.FinalMerging;
end;

{ TdxCalculateFieldResult }

constructor TdxCalculateFieldResult.Create(AValue: TdxCalculatedFieldValue;
  AUpdateType: TdxUpdateFieldOperationTypes);
begin
  inherited Create;
  FValue := AValue;
  FUpdateType := AUpdateType;
end;

destructor TdxCalculateFieldResult.Destroy;
begin
  if not (FValue.IsNull or FValue.IsInvalid) then
    FreeAndNil(FValue);
  inherited Destroy;
end;

function TdxCalculateFieldResult.GetOptions: TdxFieldResultOptions;
begin
  Result := FValue.Options;
end;

function TdxCalculateFieldResult.GetMergeFormat: Boolean;
begin
  Result := TdxFieldResultOption.MergeFormat in Options;
end;

function TdxCalculateFieldResult.GetCharFormat: Boolean;
begin
  Result := TdxFieldResultOption.CharFormat in Options;
end;

function TdxCalculateFieldResult.GetMailMergeField: Boolean;
begin
  Result := TdxFieldResultOption.MailMergeField in Options;
end;

function TdxCalculateFieldResult.GetKeepOldResult: Boolean;
begin
  Result := TdxFieldResultOption.KeepOldResult in Options;
end;

function TdxCalculateFieldResult.GetApplyFieldCodeFormatting: Boolean;
begin
  Result := not (TdxFieldResultOption.DoNotApplyFieldCodeFormatting in Options);
end;

function TdxCalculateFieldResult.GetSuppressMergeUseFirstParagraphStyle: Boolean;
begin
  Result := TdxFieldResultOption.SuppressMergeUseFirstParagraphStyle in Options;
end;

{ TdxUpdateFieldOperationResult }

class constructor TdxUpdateFieldOperationResult.Initialize;
begin
  DisabledUpdate := TdxUpdateFieldOperationResult.Create(TdxUpdateFieldResult.FieldNotUpdated, True);
end;

class destructor TdxUpdateFieldOperationResult.Finalize;
begin
  FreeAndNil(DisabledUpdate);
end;

constructor TdxUpdateFieldOperationResult.Create(AUpdateFieldResult: TdxUpdateFieldResult; ASuppressUpdateInnerCodeFields: Boolean);
begin
  inherited Create;
  FUpdateFieldResult := AUpdateFieldResult;
  FSuppressUpdateInnerCodeFields := ASuppressUpdateInnerCodeFields;
end;

{ TdxUpdateFieldOperation }

constructor TdxUpdateFieldOperation.Create(APieceTable: TdxPieceTable; AMailMergeDataMode: TdxMailMergeDataMode);
begin
  inherited Create;
  FEndResultLogPos := -1;
  FPieceTable := APieceTable;
  FMailMergeDataMode := AMailMergeDataMode;
end;

function TdxUpdateFieldOperation.GetBookmarksByEndPosition(APieceTable: TdxPieceTable; APosition: TdxDocumentLogPosition): TdxBookmarkList;
var
  ASource: TdxBookmarkCollection;
  ACount, I: Integer;
begin
  Result := TdxBookmarkList.Create;
  ASource := APieceTable.Bookmarks;
  ACount := ASource.Count;
  for I := 0 to ACount - 1 do
    if ASource[I].&End = APosition then
      Result.Add(ASource[I]);
end;

procedure TdxUpdateFieldOperation.ModifyBookmarks(APieceTable: TdxPieceTable; ABookmarks: TdxBookmarkList; ANewEndLogPosition: TdxDocumentLogPosition);
var
  ACount, I: Integer;
  ANewEndPosition: TdxDocumentModelPosition;
begin
  ACount := ABookmarks.Count;
  ANewEndPosition := TdxPositionConverter.ToDocumentModelPosition(APieceTable, ANewEndLogPosition);
  for I := 0 to ACount - 1 do
  begin
    if ABookmarks[I].&End = ABookmarks[I].Start then
      ABookmarks[I].SetStartCore(ANewEndPosition);
    ABookmarks[I].SetEndCore(ANewEndPosition);
  end;
end;

function TdxUpdateFieldOperation.GetDocumentModel: TdxDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxUpdateFieldOperation.Execute(AField: TdxField; AUpdateType: TdxUpdateFieldOperationType): TdxUpdateFieldOperationResult;
var
  AResult: TdxCalculateFieldResult;
begin
  try
    AResult := PieceTable.CalculateFieldResult(AField, FMailMergeDataMode, AUpdateType);
    try
      Result := InsertResult(AField, AResult, AUpdateType);
    finally
      AResult.Free;
    end;
  except
    Result := TdxUpdateFieldOperationResult.Create(TdxUpdateFieldResult.FieldNotUpdated, False);
  end;
end;

function TdxUpdateFieldOperation.InsertResult(AField: TdxField; AResult: TdxCalculateFieldResult;
  AUpdateType: TdxUpdateFieldOperationType): TdxUpdateFieldOperationResult;
var
  AUpdateResult: TdxUpdateFieldOperationResult;
begin
  if AResult = nil then
    Exit(TdxUpdateFieldOperationResult.Create(TdxUpdateFieldResult.FieldNotUpdated, False));
  if not (AUpdateType in AResult.UpdateType) or AResult.KeepOldResult then
    Exit(TdxUpdateFieldOperationResult.Create(TdxUpdateFieldResult.FieldNotUpdated, False));
  if TdxFieldResultOption.HyperlinkField in AResult.Options then
  begin
    if AField.IsCodeView then
      PieceTable.ToggleFieldCodes(AField);
    Exit(TdxUpdateFieldOperationResult.Create(TdxUpdateFieldResult.FieldUpdated, TdxFieldResultOption.SuppressUpdateInnerCodeFields in AResult.Options));
  end
  else
  begin
    if PieceTable.IsHyperlinkField(AField) then
      PieceTable.RemoveHyperlinkInfo(AField.Index);
  end;

  BeginFieldUpdate(AField);
  try
    AUpdateResult := ReplaceFieldResult(AResult, AUpdateType);
    if (AUpdateResult.UpdateFieldResult <> TdxUpdateFieldResult.FieldUpdatedAndCodeDeleted) and AField.IsCodeView then
      PieceTable.ToggleFieldCodes(AField);
    Exit(AUpdateResult);
  finally
    EndFieldUpdate;
  end;
end;

procedure TdxUpdateFieldOperation.BeginFieldUpdate(AField: TdxField);
var
  AResultLength: Integer;
begin
  FField := AField;
  FStartResultPos := TdxDocumentModelPosition.FromRunStart(PieceTable, AField.Result.Start);
  AResultLength := AField.GetResultLength(PieceTable);
  FEndResultLogPos := FStartResultPos.LogPosition + AResultLength - 1;

  DocumentModel.History.BeginTransaction;
end;

procedure TdxUpdateFieldOperation.EndFieldUpdate;
begin
  FField := nil;
  FStartResultPos := TdxDocumentModelPosition.Null;
  FEndResultLogPos := -1;

  DocumentModel.History.EndTransaction;
end;

function TdxUpdateFieldOperation.ReplaceFieldResult(AResult: TdxCalculateFieldResult; AUpdateType: TdxUpdateFieldOperationType): TdxUpdateFieldOperationResult;
var
  AFieldValue: TdxCalculatedFieldValue;
  ATargetModel: TdxDocumentModel;
  AShouldDeleteFieldCode, AEmptyParagraphInserted, AIsFieldSelectedBefore, AIsFieldSelectedAfter: Boolean;
  APictureFieldSize: TSize;
  AOldResultStart, AOldResultEnd: TdxDocumentModelPosition;
  AEndPositionBeforeInsert, AEndPositionAfterInsert, ANewResultEnd: TdxDocumentLogPosition;
  ABookmarksEndPosition, AOldDocumentEnd, ANewDocumentEnd: TdxDocumentLogPosition;
  ABookmarks: TdxBookmarkList;
  ASourceRun: TdxTextRunBase;
  ALength, ADelta: Integer;
begin
  AFieldValue := AResult.Value;
  if AFieldValue.IsDocumentModelValue then
    ATargetModel := TdxDocumentModel(AFieldValue.DocumentModel)
  else
    ATargetModel := nil;
  if AResult.MergeFormat then
  begin
    if not AFieldValue.IsDocumentModelValue then
    begin
      ATargetModel := PieceTable.DocumentModel.GetFieldResultModel;
      if AFieldValue.Text <> '' then
        ATargetModel.MainPieceTable.InsertPlainText(0, AFieldValue.RawValue.ToString);
    end;
    CopyCharacterFormatToDocumentModel(ATargetModel);
    CopyParagraphFormatToDocumentModel(ATargetModel);
  end
  else
  begin
    if AResult.ApplyFieldCodeFormatting then
    begin
      if ATargetModel <> nil then
        CopyFormattingFromCodeFirstChar(TdxDocumentModelPosition.Create(ATargetModel.MainPieceTable));
    end;
  end;
  try
    AShouldDeleteFieldCode := (MailMergeDataMode = TdxMailMergeDataMode.FinalMerging) and AResult.MailMergeField;
    APictureFieldSize := GetInlinePictureFieldSize;

    AOldResultStart := GetDeleteStart(AShouldDeleteFieldCode);
    AOldResultEnd := GetDeleteEnd(AShouldDeleteFieldCode);
    if AShouldDeleteFieldCode then
    begin
      AEmptyParagraphInserted := False;
      if ATargetModel = nil then
      begin
        AEndPositionBeforeInsert := PieceTable.DocumentEndLogPosition;
        PieceTable.DocumentModel.ResetMerging;
        if AFieldValue.Text <> '' then
        begin
          PieceTable.InsertPlainText(AOldResultEnd.LogPosition + 1, AFieldValue.Text);
          AEndPositionAfterInsert := PieceTable.DocumentEndLogPosition;
          if not AResult.MergeFormat and AResult.ApplyFieldCodeFormatting then
          begin
            ASourceRun := GetSourceRun;
            ALength := AEndPositionAfterInsert - AEndPositionBeforeInsert;
            CopyFormattingFromCodeFirstChar(PieceTable, ASourceRun, AOldResultEnd.LogPosition + 1, ALength);
          end;
        end;
      end
      else
      begin
        AEmptyParagraphInserted := ATargetModel.MainPieceTable.Paragraphs.First.IsInCell and
          not (PieceTable.Runs[AOldResultEnd.RunIndex] is TdxParagraphRun);
        ABookmarksEndPosition := AOldResultEnd.LogPosition + 1;
        ABookmarks := GetBookmarksByEndPosition(PieceTable, ABookmarksEndPosition);
        try
          AOldDocumentEnd := PieceTable.DocumentEndLogPosition;
          PieceTable.InsertDocumentModelContent(ATargetModel, AOldResultEnd.LogPosition + 1, True, False, True);
          ANewDocumentEnd := PieceTable.DocumentEndLogPosition;
          ADelta := ANewDocumentEnd - AOldDocumentEnd;
          ModifyBookmarks(PieceTable, ABookmarks, ABookmarksEndPosition + ADelta);
        finally
          ABookmarks.Free;
        end;
      end;
      DeleteOldFieldResult(AOldResultStart, AOldResultEnd, AResult.SuppressMergeUseFirstParagraphStyle);
      if AEmptyParagraphInserted then
        DeleteOldFieldResult(AOldResultStart, AOldResultStart, AResult.SuppressMergeUseFirstParagraphStyle);
    end
    else
    begin
      ASourceRun := GetSourceRun;
      AIsFieldSelectedBefore := IsFieldSelected(AOldResultStart.LogPosition, AOldResultEnd.LogPosition);
      DeleteOldFieldResult(AOldResultStart, AOldResultEnd, AResult.SuppressMergeUseFirstParagraphStyle);
      if ATargetModel = nil then
      begin
        AEndPositionBeforeInsert := PieceTable.DocumentEndLogPosition;
        PieceTable.DocumentModel.ResetMerging;
        if AFieldValue.Text <> '' then
        begin
          PieceTable.InsertPlainText(AOldResultStart.LogPosition, AFieldValue.Text);
          AEndPositionAfterInsert := PieceTable.DocumentEndLogPosition;
          ALength := AEndPositionAfterInsert - AEndPositionBeforeInsert;
          if not AResult.MergeFormat and AResult.ApplyFieldCodeFormatting then
            CopyFormattingFromCodeFirstChar(PieceTable, ASourceRun, AOldResultStart.LogPosition, ALength);
          ANewResultEnd := AOldResultStart.LogPosition + ALength;
          AIsFieldSelectedAfter := IsFieldSelected(AOldResultStart.LogPosition, ANewResultEnd);
          if AIsFieldSelectedBefore and not AIsFieldSelectedAfter then
          begin
            PieceTable.DocumentModel.Selection.Start := MinLogPosition(PieceTable.DocumentModel.Selection.Start, AOldResultStart.LogPosition);
            PieceTable.DocumentModel.Selection.&End := MaxLogPosition(PieceTable.DocumentModel.Selection.&End, ANewResultEnd);
          end;
        end;
      end
      else
        PieceTable.InsertDocumentModelContent(ATargetModel, AOldResultStart.LogPosition, True, True, True);
    end;

    if (AUpdateType = TdxUpdateFieldOperationType.Copy) or (AUpdateType = TdxUpdateFieldOperationType.Load) or
        (AUpdateType = TdxUpdateFieldOperationType.CreateModelForExport) then
      SetInlinePictureFieldSize(APictureFieldSize);
    if AResult.MergeFormat then
      FixLastParagraphFormatting(ATargetModel, AOldResultStart);
    if AShouldDeleteFieldCode then
      Result := TdxUpdateFieldOperationResult.Create(TdxUpdateFieldResult.FieldUpdatedAndCodeDeleted, TdxFieldResultOption.SuppressUpdateInnerCodeFields in AResult.Options)
    else
      Result := TdxUpdateFieldOperationResult.Create(TdxUpdateFieldResult.FieldUpdated, TdxFieldResultOption.SuppressUpdateInnerCodeFields in AResult.Options);
  finally
    if AResult.MergeFormat and not AFieldValue.IsDocumentModelValue then
      ATargetModel.Free;
  end;
end;

function TdxUpdateFieldOperation.IsFieldSelected(AStartPosition: TdxDocumentLogPosition; AEndPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := ((PieceTable.DocumentModel.Selection.Start <= AStartPosition) and
    (AEndPosition <= PieceTable.DocumentModel.Selection.&End)) and
    (PieceTable.DocumentModel.Selection.Start <> PieceTable.DocumentModel.Selection.&End);
end;

function TdxUpdateFieldOperation.MinLogPosition(APos1: TdxDocumentLogPosition; APos2: TdxDocumentLogPosition): TdxDocumentLogPosition;
begin
  if APos1 < APos2 then
    Exit(APos1);
  Result := APos2;
end;

function TdxUpdateFieldOperation.MaxLogPosition(APos1: TdxDocumentLogPosition; APos2: TdxDocumentLogPosition): TdxDocumentLogPosition;
begin
  if APos1 > APos2 then
    Exit(APos1);
  Result := APos2;
end;

procedure TdxUpdateFieldOperation.SetInlinePictureFieldSize(const ASize: TSize);
var
  APictureRun: TdxInlinePictureRun;
begin
  if ASize.IsEqual(cxNullSize) then
    Exit;
  if ((FField.Result.&End - FField.Result.Start) = 1) and
    (PieceTable.Runs[FField.Result.Start] is TdxInlinePictureRun) then
  begin
    APictureRun := TdxInlinePictureRun(PieceTable.Runs[FField.Result.Start]);
    APictureRun.ActualSize := ASize;
  end;
end;

function TdxUpdateFieldOperation.GetInlinePictureFieldSize: TSize;
var
  APictureRun: TdxInlinePictureRun;
begin
  if ((FField.Result.&End - FField.Result.Start) = 1) and
    (PieceTable.Runs[FField.Result.Start] is TdxInlinePictureRun) then
  begin
    APictureRun := TdxInlinePictureRun(PieceTable.Runs[FField.Result.Start]);
    Result := APictureRun.ActualSize;
  end
  else
    Result := cxNullSize;
end;

procedure TdxUpdateFieldOperation.FixLastParagraphFormatting(ATargetModel: TdxDocumentModel;
  const AResultStart: TdxDocumentModelPosition);
var
  AParagraphs: TdxParagraphCollection;
  ALastParagraph: TdxParagraph;
  ALastInsertedParIndex: TdxParagraphIndex;
begin
  AParagraphs := ATargetModel.MainPieceTable.Paragraphs;
  if AParagraphs.Count = 1 then
    Exit;
  ALastParagraph := AParagraphs.Last;
  ALastInsertedParIndex := AResultStart.ParagraphIndex + AParagraphs.Count - 1;
  CopyParagraphFormatCore(ALastParagraph, PieceTable.Paragraphs[ALastInsertedParIndex]);
end;

function TdxUpdateFieldOperation.DeleteOldFieldResult(const AStartPosition, AEndPosition: TdxDocumentModelPosition;
  ASuppressMergeUseFirstParagraphStyle: Boolean): TdxDocumentModelPosition;
var
  I, ALength: Integer;
  AOldValue: Boolean;
  AIncludedFields: TdxFieldList;
  AOperation: TdxDeleteContentOperation;
begin
  ALength := AEndPosition.LogPosition - AStartPosition.LogPosition + 1;
  DocumentModel.BeginUpdate;
  AOldValue := DocumentModel.EditingOptions.MergeUseFirstParagraphStyle;
  if ASuppressMergeUseFirstParagraphStyle then
    DocumentModel.EditingOptions.MergeUseFirstParagraphStyle := False;
  try
    AIncludedFields := PieceTable.GetEntireFieldsFromInterval(AStartPosition.RunIndex, AEndPosition.RunIndex);
    try
      for I := 0 to AIncludedFields.Count - 1 do
        PieceTable.RemoveField(AIncludedFields[I]);
    finally
      AIncludedFields.Free;
    end;
    AOperation := TdxDeleteContentOperation(PieceTable.CreateDeleteContentOperation);
    try
      AOperation.SuppressFieldDelete := True;
      AOperation.Execute(AStartPosition.LogPosition, ALength, False);
    finally
      AOperation.Free;
    end;

    PieceTable.ApplyChanges(TdxDocumentModelChangeType.DeleteContent, AStartPosition.RunIndex, MaxInt);
  finally
    DocumentModel.EditingOptions.MergeUseFirstParagraphStyle := AOldValue;
    DocumentModel.EndUpdate;
  end;
  Result := AStartPosition;
end;

procedure TdxUpdateFieldOperation.RemoveField;
begin
end;

function TdxUpdateFieldOperation.GetDeleteStart(ADeleteFieldCode: Boolean): TdxDocumentModelPosition;
begin
  if not ADeleteFieldCode then
    Result := FStartResultPos
  else
    Result := TdxDocumentModelPosition.FromRunStart(PieceTable, FField.FirstRunIndex);
end;

function TdxUpdateFieldOperation.GetDeleteEnd(ADeleteFieldCode: Boolean): TdxDocumentModelPosition;
begin
  if not ADeleteFieldCode then
    Result := TdxPositionConverter.ToDocumentModelPosition(PieceTable, FEndResultLogPos)
  else
    Result := TdxDocumentModelPosition.FromRunStart(PieceTable, FField.LastRunIndex);
end;

procedure TdxUpdateFieldOperation.CopyCharacterFormatToDocumentModel(ATargetModel: TdxDocumentModel);
var
  ASourcePos, ATargetPos: TdxDocumentModelPosition;
  ATargetEndLogPos: TdxDocumentLogPosition;
begin
  ASourcePos := FStartResultPos;
  ATargetPos := TdxDocumentModelPosition.Create(ATargetModel.MainPieceTable);
  ATargetEndLogPos := ATargetModel.MainPieceTable.DocumentEndLogPosition;
  while (ASourcePos.LogPosition <= FEndResultLogPos) and (ATargetPos.LogPosition <= ATargetEndLogPos) do
  begin
    CopyCharacterFormatToRuns(GetTextRun(ASourcePos), ATargetPos);
    ATargetPos := GetNextCharacterGroupPosition(ATargetPos);
    ASourcePos := GetNextCharacterGroupPosition(ASourcePos);
  end;
  if ATargetPos.LogPosition < ATargetEndLogPos then
    CopyFormattingFromCodeFirstChar(ATargetPos);
end;

procedure TdxUpdateFieldOperation.CopyFormattingFromCodeFirstChar(const ATargetPos: TdxDocumentModelPosition);
var
  ASourceRun: TdxTextRunBase;
  ARuns: TdxTextRunCollection;
  AEndRunIndex, I: TdxRunIndex;
begin
  ASourceRun := GetSourceRun;
  if ASourceRun = nil then
    Exit;
  ARuns := TdxTextRunCollection(ATargetPos.PieceTable.Runs);
  AEndRunIndex := ARuns.Count - 1;
  for I := ATargetPos.RunIndex to AEndRunIndex - 1 do
    CopyCharacterFormatCore(ASourceRun, ARuns[I]);
end;

procedure TdxUpdateFieldOperation.CopyFormattingFromCodeFirstChar(APieceTable: TdxPieceTable; ASourceRun: TdxTextRunBase; ATargetPos: TdxDocumentLogPosition; ALength: Integer);
var
  ARuns: TdxTextRunCollection;
  AStartIndex, AEndIndex, I: TdxRunIndex;
begin
  if (ASourceRun = nil) or (ALength = 0) then
    Exit;
  ARuns := APieceTable.Runs;
  AStartIndex := TdxPositionConverter.ToDocumentModelPosition(APieceTable, ATargetPos).RunIndex;
  AEndIndex := TdxPositionConverter.ToDocumentModelPosition(APieceTable, ATargetPos + ALength).RunIndex;
  for I := AStartIndex to AEndIndex - 1 do
    CopyCharacterFormatCore(ASourceRun, ARuns[I]);
end;

function TdxUpdateFieldOperation.GetSourceRun: TdxTextRunBase;
var
  AEndRunIndex: TdxRunIndex;
  APos: TdxDocumentModelPosition;
begin
  AEndRunIndex := FField.Code.&End;
  APos := TdxDocumentModelPosition.FromRunEnd(PieceTable, FField.Code.Start);
  while {$IFDEF DELPHIXE4}GetCharacter(APos).IsWhiteSpace{$ELSE}TCharacter.IsWhiteSpace(GetCharacter(APos)){$ENDIF} and (APos.RunIndex < AEndRunIndex) do
    TdxDocumentModelPosition.MoveForwardCore(APos);
  if APos.RunIndex <> AEndRunIndex then
    Result := PieceTable.Runs[APos.RunIndex]
  else
    Result := nil;
end;

function TdxUpdateFieldOperation.GetCharacter(const APos: TdxDocumentModelPosition): Char;
var
  ARunText: string;
begin
  ARunText := PieceTable.Runs[APos.RunIndex].GetNonEmptyText(PieceTable.TextBuffer);
  Result := ARunText[APos.RunOffset];
end;

procedure TdxUpdateFieldOperation.CopyParagraphFormatToDocumentModel(ATargetModel: TdxDocumentModel);
var
  ASourcePos, ATargetPos: TdxDocumentModelPosition;
  ATargetEndLogPos: TdxDocumentLogPosition;
  ASourceParagraph: TdxParagraph;
begin
  ASourcePos := FStartResultPos;
  ATargetPos := TdxDocumentModelPosition.Create(ATargetModel.MainPieceTable);
  ATargetEndLogPos := ATargetModel.MainPieceTable.DocumentEndLogPosition;
  ASourceParagraph := nil;
  while (ASourcePos.LogPosition <= FEndResultLogPos) and (ATargetPos.LogPosition <= ATargetEndLogPos) do
  begin
    ASourceParagraph := GetParagraph(ASourcePos);
    CopyParagraphFormatCore(ASourceParagraph, GetParagraph(ATargetPos));
    ATargetPos := GetNextParagraphPosition(ATargetPos);
    ASourcePos := GetNextParagraphPosition(ASourcePos);
  end;
  if (ATargetPos.LogPosition < ATargetEndLogPos) and (ASourceParagraph <> nil) then
    CopyParagraphFormatCore(ASourceParagraph, ATargetModel.MainPieceTable.Paragraphs.Last);
end;

procedure TdxUpdateFieldOperation.TrySplitRun(const ATo: TdxDocumentModelPosition);
var
  ADocumentModel: TdxCustomDocumentModel;
begin
  if ATo.RunOffset = 0 then
    Exit;

  ADocumentModel := ATo.PieceTable.DocumentModel;
  ADocumentModel.BeginUpdate;
  try
    TdxPieceTable(ATo.PieceTable).SplitTextRun(ATo.ParagraphIndex, ATo.RunIndex, ATo.RunOffset);
  finally
    ADocumentModel.EndUpdate;
  end;
end;

procedure TdxUpdateFieldOperation.CopyCharacterFormatToRuns(ASourceRun: TdxTextRunBase; const ATargetPos: TdxDocumentModelPosition);
var
  ATargetEnd: TdxDocumentModelPosition;
  ARuns: TdxTextRunCollection;
  I: TdxRunIndex;
begin
  ATargetEnd := GetNextCharacterGroupPosition(ATargetPos);
  TrySplitRun(ATargetEnd);
  ARuns := TdxTextRunCollection(ATargetPos.PieceTable.Runs);
  for I := ATargetPos.RunIndex to ATargetEnd.RunIndex do
    CopyCharacterFormatCore(ASourceRun, ARuns[I]);
end;

procedure TdxUpdateFieldOperation.CopyCharacterFormatCore(ASourceRun: TdxTextRunBase; ATargetRun: TdxTextRunBase);
begin
  ATargetRun.CharacterProperties.CopyFrom(ASourceRun.CharacterProperties.Info);
  ATargetRun.CharacterStyleIndex := ASourceRun.CharacterStyle.Copy(ATargetRun.Paragraph.DocumentModel);
end;

procedure TdxUpdateFieldOperation.CopyParagraphFormatCore(ASourceParagraph: TdxParagraph; ATargetParagraph: TdxParagraph);
begin
  ATargetParagraph.ParagraphProperties.CopyFrom(ASourceParagraph.ParagraphProperties.Info);
  ATargetParagraph.ParagraphStyleIndex := ASourceParagraph.ParagraphStyle.Copy(ATargetParagraph.DocumentModel);
end;

function TdxUpdateFieldOperation.GetTextRun(const APos: TdxDocumentModelPosition): TdxTextRunBase;
begin
  Result := TdxTextRunBase(APos.PieceTable.Runs[APos.RunIndex]);
end;

function TdxUpdateFieldOperation.GetParagraph(const APos: TdxDocumentModelPosition): TdxParagraph;
begin
  Result := TdxParagraph(APos.PieceTable.Paragraphs[APos.ParagraphIndex]);
end;

function TdxUpdateFieldOperation.GetNextCharacterGroupPosition(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  AIterator: TdxCharactersGroupIterator;
begin
  AIterator := TdxCharactersGroupIterator.Create(TdxPieceTable(APos.PieceTable));
  try
    Result := AIterator.MoveNext(APos);
  finally
    AIterator.Free;
  end;
end;

function TdxUpdateFieldOperation.GetNextParagraphPosition(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  AIterator: TdxParagraphsDocumentModelIterator;
begin
  AIterator := TdxParagraphsDocumentModelIterator.Create(TdxPieceTable(APos.PieceTable));
  try
    Result := AIterator.MoveForward(APos);
  finally
    AIterator.Free;
  end;
end;

{ TdxDocumentVariableCollection }

constructor TdxDocumentVariableCollection.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
  FVariables := TDictionary<string, TValue>.Create;
end;

destructor TdxDocumentVariableCollection.Destroy;
begin
  FreeAndNil(FVariables);
  inherited Destroy;
end;

procedure TdxDocumentVariableCollection.Add(const AName: string; const AValue: TValue);
begin
  FVariables.AddOrSetValue(AName, AValue);
end;

procedure TdxDocumentVariableCollection.Clear;
begin
  FVariables.Clear;
end;

function TdxDocumentVariableCollection.GetVariableNames: TDictionary<string, TValue>.TKeyCollection;
begin
  Result := FVariables.Keys;
end;

function TdxDocumentVariableCollection.GetVariableValue(const AVariableName: string;
  AArguments: TdxArgumentCollection): TValue;
var
  AArgs: TdxCalculateDocumentVariableEventArgs;
  AServer: TdxInternalRichEditDocumentServer;
  AOptions: TdxRichEditMailMergeOptions;
begin
  AArgs := TdxCalculateDocumentVariableEventArgs.Create(AVariableName, AArguments);
  try
    if TdxDocumentModel(FDocumentModel).RaiseCalculateDocumentVariable(AArgs) then
    begin
      AServer := TdxInternalRichEditDocumentServer.TryConvertInternalRichEditDocumentServer(AArgs.Value);
      if AServer <> nil then
      begin
        AOptions := AServer.DocumentModel.MailMergeOptions;
        if not AOptions.KeepLastParagraph then
          AOptions.KeepLastParagraph := AArgs.KeepLastParagraph;
      end;
      Exit(AArgs.Value);
    end;

    if not FVariables.TryGetValue(AVariableName, Result) then
      Result := nil;
  finally
    AArgs.Free;
  end;
end;

procedure TdxDocumentVariableCollection.Remove(const AName: string);
begin
  FVariables.Remove(AName);
end;

procedure TdxDocumentVariableCollection.SetVariableValue(const AVariableName: string; const AValue: TValue);
begin
  if not FVariables.ContainsKey(AVariableName) then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionVariableDeletedOrMissed));
  FVariables[AVariableName] := AValue;
end;

function TdxDocumentVariableCollection.GetCount: Integer;
begin
  Result := FVariables.Count;
end;

function TdxDocumentVariableCollection.GetItem(const AName: string): TValue;
begin
  if not FVariables.TryGetValue(AName, Result) then
    Result := nil;
end;

procedure TdxDocumentVariableCollection.SetItem(const AName: string; const Value: TValue);
begin
  SetVariableValue(AName, Value);
end;

function TdxDocumentVariableCollection.GetNames: TArray<string>;
begin
  Result := GetVariableNames.ToArray;
end;

function TdxDocumentVariableCollection.GetVariableValue(const AVariableName: string): TValue;
var
  AList: TdxArgumentCollection;
begin
  AList := TdxArgumentCollection.Create;
  try
    Result := GetVariableValue(AVariableName, AList);
  finally
    AList.Free;
  end;
end;

{ TdxFieldResultFormatting }

constructor TdxFieldResultFormatting.Create(const ANumericFormatting: TdxNullableString; const AGeneralFormatting: TArray<string>);
begin
  inherited Create;
  FNumericFormatting := ANumericFormatting;
  FGeneralFormatting := AGeneralFormatting;
end;

function TdxFieldResultFormatting.Clone: TdxFieldResultFormatting;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxFieldResultFormattingClass(ClassType).Create(FNumericFormatting, FGeneralFormatting);
end;

function TdxFieldResultFormatting.GetGeneralFormatting: TArray<string>;
begin
  Result := FGeneralFormatting;
end;

function TdxFieldResultFormatting.GetValue(AFormatter: TObject; ADocumentModel: TdxCustomDocumentModel): string;
var
  AGeneralFormattingSwitchUpperCase: string;
  AGeneralFormattingSwitch: string;
  I: Integer;
  AHasExplicitFormatting: Boolean;
  AGeneralFieldFormatter: TdxGeneralFieldFormatter;
  ACount: Integer;
  AValue: string;
  ANumericFieldFormatter: TdxNumericFieldFormatter;
  AIntValue: Integer;
  AFormat: string;
  AHasFormat: Boolean;
begin
  AIntValue := GetValueCore(AFormatter, ADocumentModel);
  try
    ANumericFieldFormatter := TdxNumericFieldFormatter.Create;
    try
      AHasFormat := not NumericFormatting.IsNull;
      if AHasFormat then
        AFormat := NumericFormatting.Value
      else
        AFormat := '';
      AValue := ANumericFieldFormatter.Format(AIntValue, AFormat, AHasFormat);
      if GeneralFormatting <> nil then
      begin
        ACount := Length(GeneralFormatting);
        AGeneralFieldFormatter := TdxGeneralFieldFormatter.Create;
        try
          AHasExplicitFormatting := False;
          for I := 0 to ACount - 1 do
          begin
            AGeneralFormattingSwitch := GeneralFormatting[I];
            AGeneralFormattingSwitchUpperCase := AnsiUpperCase(AGeneralFormattingSwitch);
            if (((AGeneralFormattingSwitchUpperCase = TdxCalculatedFieldBase.MergeFormatKeyword) or
                (AGeneralFormattingSwitchUpperCase = TdxCalculatedFieldBase.MergeFormatInetKeyword)) or
                (AGeneralFormattingSwitchUpperCase = TdxCalculatedFieldBase.CharFormatKeyword)) then
              Continue;
            AValue := AGeneralFieldFormatter.Format(AValue, AValue, AGeneralFormattingSwitch);
            AHasExplicitFormatting := True;
          end;
        finally
          AGeneralFieldFormatter.Free;
        end;
        if not AHasExplicitFormatting and (FNumericFormatting = '') then
          AValue := ApplyImplicitFormatting(AFormatter, AValue, AIntValue);
      end;
    finally
      ANumericFieldFormatter.Free;
    end;
  except
    if TdxDocumentModel(ADocumentModel).FieldOptions.ThrowExceptionOnInvalidFormatSwitch then
      raise;
    Exit(IntToStr(AIntValue));
  end;
  Result := AValue;
end;

function TdxFieldResultFormatting.ApplyImplicitFormatting(AFormatter: TObject;
  const AValue: string; AIntValue: Integer): string;
begin
  Result := AValue;
end;

end.
