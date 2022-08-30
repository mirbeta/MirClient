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

unit dxRichEdit.DocumentModel.DocumentProperties;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics, dxCoreClasses, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Core;

type

  TdxDocumentPropertiesChangeType = (
    None = 0,
    DefaultTabWidth,
    HyphenateDocument,
    DifferentOddAndEvenPages,
    PageBackColor,
    DisplayBackgroundShape,
    UpdateDocVariablesBeforePrint,
    BatchUpdate
  );

  TdxUpdateDocVariablesBeforePrint = (
    Auto,
    Always,
    Never);

  { TdxDocumentInfo }

  TdxDocumentInfo = class(TdxCloneable)
  public const
    HyphenateDocumentDefaultValue = False;
  private
    FDefaultTabWidth: Integer;
    FHyphenateDocument: Boolean;
    FDifferentOddAndEvenPages: Boolean;
    FPageBackColor: TdxAlphaColor;
    FDisplayBackgroundShape: Boolean;
    FUpdateDocVariablesBeforePrint: TdxUpdateDocVariablesBeforePrint;
  public
    constructor Create; override;
    function Clone: TdxDocumentInfo; reintroduce; inline;
    function Equals(AObject: TObject): Boolean; override;
    procedure CopyFrom(Source: TdxCloneable); override;

    property DefaultTabWidth: Integer read FDefaultTabWidth write FDefaultTabWidth;
    property HyphenateDocument: Boolean read FHyphenateDocument write FHyphenateDocument;
    property DifferentOddAndEvenPages: Boolean read FDifferentOddAndEvenPages write FDifferentOddAndEvenPages;
    property PageBackColor: TdxAlphaColor read FPageBackColor write FPageBackColor;
    property DisplayBackgroundShape: Boolean read FDisplayBackgroundShape write FDisplayBackgroundShape;
    property UpdateDocVariablesBeforePrint: TdxUpdateDocVariablesBeforePrint read FUpdateDocVariablesBeforePrint write FUpdateDocVariablesBeforePrint;
  end;

  { TdxDocumentProperties }

  TdxDocumentProperties = class(TdxRichEditIndexBasedObject<TdxDocumentInfo>)
  public const
    UpdateDocVariableFieldsBeforePrintDocVarName = '__dx_updateDocVarBeforePrint';
  private
    FOnPageBackgroundChanged: TdxEvent;
    function GetDefaultTabWidth: Integer;
    procedure SetDefaultTabWidth(const Value: Integer);
    function GetHyphenateDocument: Boolean;
    procedure SetHyphenateDocument(const Value: Boolean);
    function GetDifferentOddAndEvenPages: Boolean;
    procedure SetDifferentOddAndEvenPages(const Value: Boolean);
    function GetPageBackColor: TdxAlphaColor;
    procedure SetPageBackColor(const Value: TdxAlphaColor);
    function GetDisplayBackgroundShape: Boolean;
    procedure SetDisplayBackgroundShape(const Value: Boolean);
    function GetUpdateDocVariablesBeforePrint: TdxUpdateDocVariablesBeforePrint;
    procedure SetUpdateDocVariablesBeforePrint(const Value: TdxUpdateDocVariablesBeforePrint);
  strict protected
    function SetDefaultTabWidthCore(const AInfo: TdxDocumentInfo;
      const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetHyphenateDocumentCore(const AInfo: TdxDocumentInfo;
      const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetDifferentOddAndEvenPagesCore(const AInfo: TdxDocumentInfo;
      const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetPageBackColorCore(const AInfo: TdxDocumentInfo;
      const AValue: TdxAlphaColor): TdxDocumentModelChangeActions; virtual;
    function SetDisplayBackgroundShapeCore(const AInfo: TdxDocumentInfo;
      const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetUpdateDocVariablesBeforePrintCore(const AInfo: TdxDocumentInfo;
      const AValue: TdxUpdateDocVariablesBeforePrint): TdxDocumentModelChangeActions; virtual;
    procedure RaisePageBackgroundChanged; virtual;
    procedure OnIndexChanged; override;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxDocumentInfo>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;

    function GetUpdateFieldsBeforePrintDocVarValue: string; virtual;
    procedure SetUpdateFieldsBeforePrintFromDocVar(const AValue: string); virtual;

    property DefaultTabWidth: Integer read GetDefaultTabWidth write SetDefaultTabWidth;
    property HyphenateDocument: Boolean read GetHyphenateDocument write SetHyphenateDocument;
    property DifferentOddAndEvenPages: Boolean read GetDifferentOddAndEvenPages write SetDifferentOddAndEvenPages;
    property PageBackColor: TdxAlphaColor read GetPageBackColor write SetPageBackColor;
    property DisplayBackgroundShape: Boolean read GetDisplayBackgroundShape write SetDisplayBackgroundShape;
    property UpdateDocVariablesBeforePrint: TdxUpdateDocVariablesBeforePrint read GetUpdateDocVariablesBeforePrint write SetUpdateDocVariablesBeforePrint;
    property OnPageBackgroundChanged: TdxEvent read FOnPageBackgroundChanged write FOnPageBackgroundChanged;
  end;

  { TdxDocumentPropertiesChangeActionsCalculator }

  TdxDocumentPropertiesChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxDocumentPropertiesChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxDocumentInfoCache }

  TdxDocumentInfoCache = class(TdxUniqueItemsCache<TdxDocumentInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxDocumentInfo; override;
  end;

  TdxEncryptionPasswordQueryEvent = function (Sender: TObject; var APassword: string): Boolean of object;

  { TdxDocumentEncryptionProperties }

  TdxDocumentEncryptionProperties = class
  strict private
    FPassword: string;
    FTryCount: Integer;
    FOwner: TdxCustomDocumentModel;
  strict protected
    property Owner: TdxCustomDocumentModel read FOwner;
  public
    constructor Create(AOwner: TdxCustomDocumentModel);
    function IsEncrypted: Boolean;
    property Password: string read FPassword write FPassword;
    property TryCount: Integer read FTryCount write FTryCount;
  end;

  { IdxDocumentEncryptionPropertiesContainer }

  IdxDocumentEncryptionPropertiesContainer = interface
  ['{B11045E1-B14E-44BB-9B17-106384348AA4}']
    function GetEncryptionProperties: TdxDocumentEncryptionProperties;

    property EncryptionProperties: TdxDocumentEncryptionProperties read GetEncryptionProperties;
  end;

implementation

uses
  RTLConsts,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.DocumentModel.Cache.Simple;

{ TdxDocumentInfo }

constructor TdxDocumentInfo.Create;
begin
  inherited Create;
  FHyphenateDocument := HyphenateDocumentDefaultValue;
end;

function TdxDocumentInfo.Clone: TdxDocumentInfo;
begin
  Result := TdxDocumentInfo(inherited Clone);
end;

procedure TdxDocumentInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxDocumentInfo absolute Source;
begin
  DefaultTabWidth := AInfo.DefaultTabWidth;
  HyphenateDocument := AInfo.HyphenateDocument;
  DifferentOddAndEvenPages := AInfo.DifferentOddAndEvenPages;
  PageBackColor := AInfo.PageBackColor;
  DisplayBackgroundShape := AInfo.DisplayBackgroundShape;
  UpdateDocVariablesBeforePrint := AInfo.UpdateDocVariablesBeforePrint;
end;

function TdxDocumentInfo.Equals(AObject: TObject): Boolean;
var
  AInfo: TdxDocumentInfo absolute AObject;
begin
  Assert(AObject is TdxDocumentInfo);
  Result := (DefaultTabWidth = AInfo.DefaultTabWidth) and
    (HyphenateDocument = AInfo.HyphenateDocument) and
    (DifferentOddAndEvenPages = AInfo.DifferentOddAndEvenPages) and
    (PageBackColor = AInfo.PageBackColor) and
    (DisplayBackgroundShape = AInfo.DisplayBackgroundShape) and
    (UpdateDocVariablesBeforePrint = AInfo.UpdateDocVariablesBeforePrint);
end;

{ TdxDocumentProperties }

constructor TdxDocumentProperties.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel.MainPart);
end;

function TdxDocumentProperties.GetUpdateFieldsBeforePrintDocVarValue: string;
const
  AResultMap: array[TdxUpdateDocVariablesBeforePrint] of string = ('', 'always', 'never');
begin
  Result := AResultMap[UpdateDocVariablesBeforePrint];
end;

procedure TdxDocumentProperties.SetUpdateFieldsBeforePrintFromDocVar(const AValue: string);
begin
  if AValue = 'never' then
    UpdateDocVariablesBeforePrint := TdxUpdateDocVariablesBeforePrint.Never
  else if AValue = 'always' then
    UpdateDocVariablesBeforePrint := TdxUpdateDocVariablesBeforePrint.Always
  else
    UpdateDocVariablesBeforePrint := TdxUpdateDocVariablesBeforePrint.Auto;
end;

function TdxDocumentProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxDocumentPropertiesChangeActionsCalculator.CalculateChangeActions(TdxDocumentPropertiesChangeType.BatchUpdate);
end;

function TdxDocumentProperties.GetCache(
  const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxDocumentInfo>;
begin
  Result := TdxSimpleDocumentCache(ADocumentModel.Cache).DocumentInfoCache;
end;

function TdxDocumentProperties.GetDefaultTabWidth: Integer;
begin
  Result := Info.DefaultTabWidth;
end;

function TdxDocumentProperties.GetDifferentOddAndEvenPages: Boolean;
begin
  Result := Info.DifferentOddAndEvenPages;
end;

function TdxDocumentProperties.GetDisplayBackgroundShape: Boolean;
begin
  Result := Info.DisplayBackgroundShape;
end;

function TdxDocumentProperties.GetHyphenateDocument: Boolean;
begin
  Result := Info.HyphenateDocument;
end;

function TdxDocumentProperties.GetPageBackColor: TdxAlphaColor;
begin
  Result := Info.PageBackColor;
end;

procedure TdxDocumentProperties.OnIndexChanged;
begin
  inherited OnIndexChanged;
  RaisePageBackgroundChanged;
end;

procedure TdxDocumentProperties.RaisePageBackgroundChanged;
begin
  if Assigned(FOnPageBackgroundChanged) then
    FOnPageBackgroundChanged(Self, nil);
end;

procedure TdxDocumentProperties.SetDefaultTabWidth(const Value: Integer);
begin
  Assert(Value > 0);
  if DefaultTabWidth = Value then
    Exit;
  SetPropertyValue<Integer>(SetDefaultTabWidthCore, Value);
end;

function TdxDocumentProperties.SetDefaultTabWidthCore(const AInfo: TdxDocumentInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.DefaultTabWidth := AValue;
  Result := TdxDocumentPropertiesChangeActionsCalculator.CalculateChangeActions(TdxDocumentPropertiesChangeType.DefaultTabWidth);
end;

procedure TdxDocumentProperties.SetDifferentOddAndEvenPages(const Value: Boolean);
begin
  if DifferentOddAndEvenPages = Value then
    Exit;
  SetPropertyValue<Boolean>(SetDifferentOddAndEvenPagesCore, Value);
end;

function TdxDocumentProperties.SetDifferentOddAndEvenPagesCore(const AInfo: TdxDocumentInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.DifferentOddAndEvenPages := AValue;
  Result := TdxDocumentPropertiesChangeActionsCalculator.CalculateChangeActions(TdxDocumentPropertiesChangeType.DifferentOddAndEvenPages);
end;

procedure TdxDocumentProperties.SetDisplayBackgroundShape(const Value: Boolean);
begin
  if DisplayBackgroundShape = Value then
    Exit;
  SetPropertyValue<Boolean>(SetDisplayBackgroundShapeCore, Value);
end;

function TdxDocumentProperties.GetUpdateDocVariablesBeforePrint: TdxUpdateDocVariablesBeforePrint;
begin
  Result := Info.UpdateDocVariablesBeforePrint;
end;

procedure TdxDocumentProperties.SetUpdateDocVariablesBeforePrint(const Value: TdxUpdateDocVariablesBeforePrint);
begin
  if UpdateDocVariablesBeforePrint = Value then
    Exit;
  SetPropertyValue<TdxUpdateDocVariablesBeforePrint>(SetUpdateDocVariablesBeforePrintCore, Value);
end;

function TdxDocumentProperties.SetDisplayBackgroundShapeCore(const AInfo: TdxDocumentInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.DisplayBackgroundShape := AValue;
  Result := TdxDocumentPropertiesChangeActionsCalculator.CalculateChangeActions(TdxDocumentPropertiesChangeType.DisplayBackgroundShape);
end;

function TdxDocumentProperties.SetUpdateDocVariablesBeforePrintCore(const AInfo: TdxDocumentInfo;
  const AValue: TdxUpdateDocVariablesBeforePrint): TdxDocumentModelChangeActions;
begin
  AInfo.UpdateDocVariablesBeforePrint := AValue;
  Result := TdxDocumentPropertiesChangeActionsCalculator.CalculateChangeActions(TdxDocumentPropertiesChangeType.UpdateDocVariablesBeforePrint);
end;

procedure TdxDocumentProperties.SetHyphenateDocument(const Value: Boolean);
begin
  if HyphenateDocument = Value then
    Exit;
  SetPropertyValue<Boolean>(SetHyphenateDocumentCore, Value);
end;

function TdxDocumentProperties.SetHyphenateDocumentCore(const AInfo: TdxDocumentInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.HyphenateDocument := AValue;
  Result := TdxDocumentPropertiesChangeActionsCalculator.CalculateChangeActions(TdxDocumentPropertiesChangeType.HyphenateDocument);
end;

procedure TdxDocumentProperties.SetPageBackColor(const Value: TdxAlphaColor);
begin
  if PageBackColor = Value then
    Exit;
  SetPropertyValue<TdxAlphaColor>(SetPageBackColorCore, Value);
end;

function TdxDocumentProperties.SetPageBackColorCore(const AInfo: TdxDocumentInfo;
  const AValue: TdxAlphaColor): TdxDocumentModelChangeActions;
begin
  AInfo.PageBackColor := AValue;
  Result := TdxDocumentPropertiesChangeActionsCalculator.CalculateChangeActions(TdxDocumentPropertiesChangeType.PageBackColor);
end;

{ TdxDocumentPropertiesChangeActionsCalculator }

class function TdxDocumentPropertiesChangeActionsCalculator.CalculateChangeActions(
  AChange: TdxDocumentPropertiesChangeType): TdxDocumentModelChangeActions;
const
  DocumentPropertiesChangeActionsMap: array[TdxDocumentPropertiesChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout]
  );
begin
  Result := DocumentPropertiesChangeActionsMap[AChange];
end;

{ TdxDocumentInfoCache }

function TdxDocumentInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxDocumentInfo;
begin
  Result := TdxDocumentInfo.Create;
  Result.DefaultTabWidth := AUnitConverter.TwipsToModelUnits(720);
  Result.HyphenateDocument := TdxDocumentInfo.HyphenateDocumentDefaultValue;
end;

{ TdxDocumentEncryptionProperties }

constructor TdxDocumentEncryptionProperties.Create(AOwner: TdxCustomDocumentModel);
begin
  inherited Create;
  FOwner := AOwner;
  FTryCount := 1;
end;

function TdxDocumentEncryptionProperties.IsEncrypted: Boolean;
begin
  Result := FPassword <> '';
end;

end.
