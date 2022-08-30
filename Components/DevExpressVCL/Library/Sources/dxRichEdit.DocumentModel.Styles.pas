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

unit dxRichEdit.DocumentModel.Styles;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, Contnrs,
  dxCore, dxCoreClasses, cxClasses,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.TabFormatting;

type
  TdxStyleLinkManager = class;
  TdxCharacterStyleCollection = class;
  TdxParagraphStyleCollection = class;

  { IdxTabPropertiesContainer }

  IdxTabPropertiesContainer = interface
  ['{CD8694E9-15B5-4722-BCF3-DB42E93F7772}']
    function GetDocumentModel: TdxCustomDocumentModel;

    function CreateTabPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnTabPropertiesChanged;

    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
  end;

  { IdxStylesContainer }

  IdxStylesContainer = interface
  ['{95174952-A511-4ED2-B390-9718CD7D98B5}']
    function GetCharacterStyles: TdxCharacterStyleCollection;
    function GetDefaultCharacterProperties: TdxCharacterProperties;
    function GetDefaultParagraphProperties: TdxParagraphProperties;
    function GetParagraphStyles: TdxParagraphStyleCollection;
    function GetStyleLinkManager: TdxStyleLinkManager;

    property CharacterStyles: TdxCharacterStyleCollection read GetCharacterStyles;
    property DefaultCharacterProperties: TdxCharacterProperties read GetDefaultCharacterProperties;
    property DefaultParagraphProperties: TdxParagraphProperties read GetDefaultParagraphProperties;
    property ParagraphStyles: TdxParagraphStyleCollection read GetParagraphStyles;
    property StyleLinkManager: TdxStyleLinkManager read GetStyleLinkManager;
  end;

  { TdxParagraphPropertiesBasedStyle }

  TdxParagraphPropertiesBasedStyle = class(TdxStyleBase, IdxParagraphPropertiesContainer, IdxTabPropertiesContainer)
  private
    FContainer: IdxStylesContainer;
    FParagraphProperties: TdxParagraphProperties;
    FTabs: TdxTabProperties;
    FFrameProperties: TdxFrameProperties;
    function GetPieceTable: TdxCustomPieceTable;
    procedure SetFrameProperties(const Value: TdxFrameProperties);
  protected
    function CalculateChangeActions: TdxDocumentModelChangeActions; override;
    function GetParentStyle: TdxParagraphPropertiesBasedStyle;
    procedure SetParentStyle(const Value: TdxParagraphPropertiesBasedStyle);
    function CreateParagraphPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnParagraphPropertiesChanged;
    function GetParagraphProperties: TdxParagraphProperties; overload;
    function GetDocumentModel: TdxCustomDocumentModel;
    function CreateTabPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnTabPropertiesChanged;

    procedure MergePropertiesWithParent; override;

    property Container: IdxStylesContainer read FContainer;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; AParent: TdxParagraphPropertiesBasedStyle = nil;
      const AStyleName: string = ''); reintroduce; virtual;
    destructor Destroy; override;

    procedure CreateFrameProperties;
    procedure CopyProperties(ASource: TdxStyleBase); override;
    function GetParagraphProperties(AMask: TdxUsedParagraphFormattingOption): TdxParagraphProperties; overload; virtual;
    function GetMergedParagraphProperties: TdxMergedParagraphProperties; virtual;
    function GetMergedFrameProperties: TdxMergedFrameProperties; virtual;
    function GetMergedWithDefaultParagraphProperties: TdxMergedParagraphProperties; virtual;
    function GetTabs: TdxTabFormattingInfo;

    property Tabs: TdxTabProperties read FTabs;
    property ParagraphProperties: TdxParagraphProperties read GetParagraphProperties;
    property Parent: TdxParagraphPropertiesBasedStyle read GetParentStyle write SetParentStyle;
    property PieceTable: TdxCustomPieceTable read GetPieceTable;
    property FrameProperties: TdxFrameProperties read FFrameProperties write SetFrameProperties;
  end;

  TdxCharacterStyle = class;

  TdxParagraphStyle = class(TdxParagraphPropertiesBasedStyle, IdxParagraphPropertiesContainer,
    IdxCharacterPropertiesContainer)
  private
    FAutoUpdate: Boolean;
    FCharacterProperties: TdxCharacterProperties;
    FListLevelIndex: Integer;
    FNextParagraphStyle: TdxParagraphStyle;
    FNumberingListIndex: TdxNumberingListIndex;
    FTabs: TdxTabProperties;
    function GetLinkedStyle: TdxCharacterStyle;
    function GetParent: TdxParagraphStyle;
    procedure SetNextParagraphStyle(Value: TdxParagraphStyle);
    procedure SetParent(const Value: TdxParagraphStyle);
    procedure SetAutoUpdate(const Value: Boolean);
  protected
    //IdxCharacterPropertiesContainer and IdxParagraphPropertiesContainer
    function GetPieceTable: TdxCustomPieceTable;
    function CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    function CreateParagraphPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    function GetType: TdxStyleType; override;
    procedure MergePropertiesWithParent; override;
    procedure OnCharacterPropertiesChanged;
    procedure OnParagraphPropertiesChanged;

    function CopyFrom(ATargetModel: TdxCustomDocumentModel): TdxParagraphStyle; virtual;
    procedure SetAutoUpdateCore(AAutoUpdate: Boolean);

    procedure SubscribeCharacterPropertiesEvents; virtual;
    procedure SubscribeParagraphPropertiesEvents; virtual;
    procedure OnObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; AParentStyle: TdxParagraphStyle = nil; const AStyleName: string = ''); reintroduce;
    destructor Destroy; override;

    function Copy(ATargetModel: TdxCustomDocumentModel): Integer; override;
    procedure CopyTo(AStyle: TdxParagraphStyle);
    procedure CopyProperties(ASource: TdxStyleBase); override;
    function GetListLevelIndex: Integer;
    function GetMergedCharacterProperties: TdxMergedCharacterProperties;
    function GetMergedWithDefaultCharacterProperties: TdxMergedCharacterProperties; virtual;
    function GetNumberingListIndex: TdxNumberingListIndex; virtual;
    function GetOwnListLevelIndex: Integer;
    function GetOwnNumberingListIndex: Integer;
    function GetTabs: TdxTabFormattingInfo;
    function HasLinkedStyle: Boolean;
    procedure SetNextParagraphStyleCore(Value: TdxParagraphStyle);
    procedure SetNumberingListIndex(ANumberingListIndex: TdxNumberingListIndex); virtual;
    procedure SetNumberingListLevelIndex(AListLevelIndex: Integer); virtual;

    property AutoUpdate: Boolean read FAutoUpdate write SetAutoUpdate;
    property CharacterProperties: TdxCharacterProperties read FCharacterProperties;
    property LinkedStyle: TdxCharacterStyle read GetLinkedStyle;
    property NextParagraphStyle: TdxParagraphStyle read FNextParagraphStyle write SetNextParagraphStyle;
    property Parent: TdxParagraphStyle read GetParent write SetParent;
    property Tabs: TdxTabProperties read FTabs;
  end;

  TdxParagraphStyleCollection = class(TdxStyleCollectionBase)
  public const
    DefaultParagraphStyleName = 'Normal';
    EmptyParagraphStyleIndex = 0;
  private
    function CreateHeadingParagraphStyle(const AStyleName: string; ALevel: Integer): Integer;
    function GetItem(Index: Integer): TdxParagraphStyle;
    function GetDefaultItem: TdxParagraphStyle;
    function GetHeadingParagraphStyleCore(ALevel: Integer): Integer;
    function GetTocStyleName(ALevel: Integer): string;
    procedure SetupHeadingParagraphStyle(AStyle: TdxParagraphStyle; ALevel: Integer);
    procedure SetupTocStyle(AStyle: TdxParagraphStyle; ALevel: Integer);
  protected
    function CreateDefaultItem: TdxStyleBase; override;
    procedure NotifyDocumentStyleDeleting(AParagraphStyle: TdxStyleBase); override;
    procedure NotifyPieceTableStyleDeleting(APieceTable: TdxCustomPieceTable; AParagraphStyle: TdxStyleBase); override;
  public
    function CreateTocStyle(ALevel: Integer): Integer;
    function GetHeadingParagraphStyle(ALevel: Integer): Integer;
    function GetTocStyle(ALevel: Integer): Integer;

    property DefaultItem: TdxParagraphStyle read GetDefaultItem;
    property Items[Index: Integer]: TdxParagraphStyle read GetItem; default;
  end;

  TdxCharacterStyle = class(TdxStyleBase, IdxCharacterPropertiesContainer)
  private
    FCharacterProperties: TdxCharacterProperties;
    FContainer: IdxStylesContainer;
    procedure OnCharacterPropertiesChangedCore;

    function GetHasLinkedStyle: Boolean;
    function GetLinkedStyle: TdxParagraphStyle;
    function GetParent: TdxCharacterStyle;
    procedure SetParent(const Value: TdxCharacterStyle);
  protected
    function CalculateChangeActions: TdxDocumentModelChangeActions; override;
    //IdxCharacterPropertiesContainer
    function GetPieceTable: TdxCustomPieceTable;
    function CopyFrom(ATargetModel: TdxCustomDocumentModel): TdxCharacterStyle; virtual;
    function CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    function GetType: TdxStyleType; override;
    procedure MergePropertiesWithParent; override;
    procedure OnCharacterPropertiesChanged;

    property Container: IdxStylesContainer read FContainer;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; AParentStyle: TdxStyleBase = nil; const AStyleName: string = ''); override;
    destructor Destroy; override;

    function GetMergedWithDefaultCharacterProperties: TdxMergedCharacterProperties; virtual;
    function Copy(ATargetModel: TdxCustomDocumentModel): Integer; override;
    procedure CopyTo(AStyle: TdxCharacterStyle);
    procedure CopyProperties(ASource: TdxStyleBase); override;
    function GetMergedCharacterProperties: TdxMergedCharacterProperties;

    property CharacterProperties: TdxCharacterProperties read FCharacterProperties;
    property HasLinkedStyle: Boolean read GetHasLinkedStyle;
    property LinkedStyle: TdxParagraphStyle read GetLinkedStyle;
    property Parent: TdxCharacterStyle read GetParent write SetParent;
  end;

  { TdxCharacterStyleCollection }

  TdxCharacterStyleCollection = class(TdxStyleCollectionBase)
  public const
    EmptyCharacterStyleIndex = 0;
    DefaultCharacterStyleName = 'Default Paragraph Font';
    LineNumberingStyleName = 'Line Number';
    HyperlinkStyleName = 'Hyperlink';
  private
    function GetDefaultItem: TdxCharacterStyle;
    function GetItem(Index: Integer): TdxCharacterStyle;
  protected
    function CreateDefaultItem: TdxStyleBase; override;
    procedure NotifyPieceTableStyleDeleting(APieceTable: TdxCustomPieceTable; AStyle: TdxStyleBase); override;
    function CreateHyperlinkStyle: TdxCharacterStyle; virtual;
    function CreateLineNumberingStyle: TdxCharacterStyle; virtual;

    function CreateHeadingCharacterStyle(const AStyleName: string; ALevel: Integer): Integer;
    function GetHeadingCharacterStyleCore(ALevel: Integer): Integer;
    procedure SetupHeadingCharacterStyle(AStyle: TdxCharacterStyle; ALevel: Integer);
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); override;

    property DefaultItem: TdxCharacterStyle read GetDefaultItem;
    property Items[Index: Integer]: TdxCharacterStyle read GetItem; default;
  end;

  { TdxStyleLinkManager }

  TdxStyleLinkManager = class
  private
    FDocumentModel: TdxCustomDocumentModel;
    FParagraphStyleToCharacterStyleLinks: TDictionary<TdxParagraphStyle, TdxCharacterStyle>;
    FCharacterStyleToParagraphStyleLinks: TDictionary<TdxCharacterStyle, TdxParagraphStyle>;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel);
    destructor Destroy; override;

    procedure CreateLink(AParagraphStyle: TdxParagraphStyle; ACharacterStyle: TdxCharacterStyle);
    procedure CreateLinkCore(AParagraphStyle: TdxParagraphStyle; ACharacterStyle: TdxCharacterStyle);
    procedure DeleteLinkCore(AParagraphStyle: TdxParagraphStyle; ACharacterStyle: TdxCharacterStyle);

    procedure DeleteLink(ACharacterStyle: TdxCharacterStyle); overload;
    procedure DeleteLink(AParagraphStyle: TdxParagraphStyle); overload;

    function GetLinkedParagraphStyle(ACharacterStyle: TdxCharacterStyle): TdxParagraphStyle;
    function GetLinkedCharacterStyle(AParagraphStyle: TdxParagraphStyle): TdxCharacterStyle;
    function HasLinkedParagraphStyle(ACharacterStyle: TdxCharacterStyle): Boolean;
    function HasLinkedCharacterStyle(AParagraphStyle: TdxParagraphStyle): Boolean;

    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
  end;

  { TdxStyleTopologicalComparer }

  TdxStyleTopologicalComparer<T: TdxStyleBase> = class(TInterfacedObject, IComparer<T>)
  public
    //IComparer
    function Compare(const Left, Right: T): Integer;
  end;

implementation

uses
  RTLConsts,
  dxCoreGraphics,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.History.Style,
  dxRichEdit.DocumentModel.UnitConverter;

type
  { TdxStyleLinkHistoryItemBase }

  TdxStyleLinkHistoryItemBase = class abstract(TdxRichEditHistoryItem)
  strict private
    FCharacterStyle: TdxCharacterStyle;
    FContainer: IdxStylesContainer;
    FParagraphStyle: TdxParagraphStyle;
  public
    constructor Create(AParagraphStyle: TdxParagraphStyle; ACharacterStyle: TdxCharacterStyle); reintroduce;

    property CharacterStyle: TdxCharacterStyle read FCharacterStyle;
    property Container: IdxStylesContainer read FContainer;
    property ParagraphStyle: TdxParagraphStyle read FParagraphStyle;
  end;

  { TdxCreateStyleLinkHistoryItem }

  TdxCreateStyleLinkHistoryItem = class(TdxStyleLinkHistoryItemBase)
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  end;

  { TdxDeleteStyleLinkHistoryItem }

  TdxDeleteStyleLinkHistoryItem = class(TdxStyleLinkHistoryItemBase)
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  end;

{ TdxStyleLinkHistoryItemBase }

constructor TdxStyleLinkHistoryItemBase.Create(AParagraphStyle: TdxParagraphStyle; ACharacterStyle: TdxCharacterStyle);
begin
  inherited Create(AParagraphStyle.DocumentModel.MainPart);
  FContainer := AParagraphStyle.DocumentModel as IdxStylesContainer;
  Assert(AParagraphStyle <> nil);
  Assert(ACharacterStyle <> nil);
  FCharacterStyle := ACharacterStyle;
  FParagraphStyle := AParagraphStyle;
end;

{ TdxCreateStyleLinkHistoryItem }

procedure TdxCreateStyleLinkHistoryItem.UndoCore;
begin
  Container.StyleLinkManager.DeleteLinkCore(ParagraphStyle, CharacterStyle);
end;

procedure TdxCreateStyleLinkHistoryItem.RedoCore;
begin
  Container.StyleLinkManager.CreateLinkCore(ParagraphStyle, CharacterStyle);
end;

{ TdxDeleteStyleLinkHistoryItem }

procedure TdxDeleteStyleLinkHistoryItem.UndoCore;
begin
  Container.StyleLinkManager.CreateLinkCore(ParagraphStyle, CharacterStyle);
end;

procedure TdxDeleteStyleLinkHistoryItem.RedoCore;
begin
  Container.StyleLinkManager.DeleteLinkCore(ParagraphStyle, CharacterStyle);
end;

{ TdxParagraphPropertiesBasedStyle }

constructor TdxParagraphPropertiesBasedStyle.Create(ADocumentModel: TdxCustomDocumentModel;
  AParent: TdxParagraphPropertiesBasedStyle = nil; const AStyleName: string = '');
begin
  inherited Create(ADocumentModel, AParent, AStyleName);
  FContainer := ADocumentModel as IdxStylesContainer;
  FParagraphProperties := TdxParagraphProperties.Create(Self);
  FTabs := TdxTabProperties.Create(Self);
end;

destructor TdxParagraphPropertiesBasedStyle.Destroy;
begin
  FreeAndNil(FTabs);
  FreeAndNil(FParagraphProperties);
  FreeAndNil(FFrameProperties);
  inherited Destroy;
end;

procedure TdxParagraphPropertiesBasedStyle.CreateFrameProperties;
begin
  if FFrameProperties = nil then
    FFrameProperties := TdxFrameProperties.Create(Self);
end;

function TdxParagraphPropertiesBasedStyle.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := inherited DocumentModel;
end;

function TdxParagraphPropertiesBasedStyle.GetMergedFrameProperties: TdxMergedFrameProperties;
var
  AMergedFrameProperties: TdxMergedFrameProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionUseDeletedStyleError));
  if FrameProperties = nil then
    Exit(nil);
  Result := TdxMergedFrameProperties.Create(FrameProperties);
  if Parent <> nil then
  begin
    AMergedFrameProperties := Parent.GetMergedFrameProperties;
    try
      Result.Merge(AMergedFrameProperties);
    finally
      AMergedFrameProperties.Free;
    end;
  end;
end;

function TdxParagraphPropertiesBasedStyle.GetMergedParagraphProperties: TdxMergedParagraphProperties;
var
  AMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  Assert(not Deleted);
  Result := TdxMergedParagraphProperties.Create(ParagraphProperties);
  if Parent <> nil then
  begin
    AMergedParagraphProperties := Parent.GetMergedParagraphProperties;
    try
      Result.Merge(AMergedParagraphProperties);
    finally
      AMergedParagraphProperties.Free;
    end;
  end;
end;

function TdxParagraphPropertiesBasedStyle.GetMergedWithDefaultParagraphProperties: TdxMergedParagraphProperties;
begin
  Assert(not Deleted);
  Result := GetMergedParagraphProperties;
  Result.Merge(Container.DefaultParagraphProperties);
end;

function TdxParagraphPropertiesBasedStyle.GetParagraphProperties(AMask: TdxUsedParagraphFormattingOption): TdxParagraphProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionUseDeletedStyleError));
  if ParagraphProperties.UseVal(AMask) then
    Exit(ParagraphProperties);
  if Parent <> nil then
    Result := Parent.GetParagraphProperties(AMask)
  else
    Result := nil;
end;

function TdxParagraphPropertiesBasedStyle.GetParagraphProperties: TdxParagraphProperties;
begin
  Result := FParagraphProperties;
end;

function TdxParagraphPropertiesBasedStyle.CalculateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.ParagraphStyle);
end;

function TdxParagraphPropertiesBasedStyle.GetParentStyle: TdxParagraphPropertiesBasedStyle;
begin
  Result := TdxParagraphPropertiesBasedStyle(inherited Parent);
end;

function TdxParagraphPropertiesBasedStyle.GetPieceTable: TdxCustomPieceTable;
begin
  Result := inherited PieceTable;
end;

function TdxParagraphPropertiesBasedStyle.GetTabs: TdxTabFormattingInfo;
var
  AOwnTabs: TdxTabFormattingInfo;
begin
  if Parent = nil then
    Exit(Tabs.GetTabs);
  AOwnTabs := Tabs.Info;
  if AOwnTabs.Count = 0 then
    Exit(Parent.GetTabs);
  Result := TdxTabFormattingInfo.Merge(AOwnTabs, Parent.GetTabs);
end;

procedure TdxParagraphPropertiesBasedStyle.CopyProperties(ASource: TdxStyleBase);
var
  ASourceStyle: TdxParagraphPropertiesBasedStyle absolute ASource;
begin
  Assert(ASource is TdxParagraphPropertiesBasedStyle);
  ParagraphProperties.CopyFrom(ASourceStyle.ParagraphProperties.Info);
  if (FrameProperties <> nil) and (ASourceStyle.FrameProperties <> nil) then
    FrameProperties.CopyFrom(ASourceStyle.FrameProperties.Info);
end;

function TdxParagraphPropertiesBasedStyle.CreateParagraphPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(DocumentModel.MainPart, ParagraphProperties);
end;

function TdxParagraphPropertiesBasedStyle.CreateTabPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(DocumentModel.MainPart, Tabs);
end;

procedure TdxParagraphPropertiesBasedStyle.MergePropertiesWithParent;
begin
  ParagraphProperties.Merge(Parent.ParagraphProperties);
  FrameProperties.Merge(Parent.FrameProperties);
end;

procedure TdxParagraphPropertiesBasedStyle.OnParagraphPropertiesChanged;
begin
  BeginUpdate;
  try
    NotifyStyleChanged;
  finally
    EndUpdate;
  end;
end;

procedure TdxParagraphPropertiesBasedStyle.OnTabPropertiesChanged;
begin
end;

procedure TdxParagraphPropertiesBasedStyle.SetFrameProperties(const Value: TdxFrameProperties);
begin
  if FFrameProperties <> Value then
  begin
    FFrameProperties.Free;
    FFrameProperties := Value;
  end;
end;

procedure TdxParagraphPropertiesBasedStyle.SetParentStyle(const Value: TdxParagraphPropertiesBasedStyle);
begin
  inherited Parent := Value;
end;

{ TdxParagraphStyle }

constructor TdxParagraphStyle.Create(ADocumentModel: TdxCustomDocumentModel; AParentStyle: TdxParagraphStyle = nil;
  const AStyleName: string = '');
begin
  inherited Create(ADocumentModel, AParentStyle, AStyleName);
  FNumberingListIndex := NumberingListIndexListIndexNotSetted;
  FCharacterProperties := TdxCharacterProperties.Create(Self);
  FTabs := TdxTabProperties.Create(Self);
  SubscribeCharacterPropertiesEvents;
  SubscribeParagraphPropertiesEvents;
end;

destructor TdxParagraphStyle.Destroy;
begin
  FreeAndNil(FTabs);
  FreeAndNil(FCharacterProperties);
  inherited Destroy;
end;

function TdxParagraphStyle.Copy(ATargetModel: TdxCustomDocumentModel): Integer;
var
  I: Integer;
  ACopy: TdxParagraphStyle;
  ATarget: IdxStylesContainer;
begin
  ATarget := ATargetModel as IdxStylesContainer;
  for I := 0 to ATarget.ParagraphStyles.Count - 1 do
  begin
    if StyleName = ATarget.ParagraphStyles[I].StyleName then
    begin
      Result := I;
      Exit;
    end;
  end;
  ACopy := CopyFrom(ATargetModel);
  ACopy.CopyProperties(Self);
  Result := ATarget.ParagraphStyles.AddNewStyle(ACopy);
  if NextParagraphStyle <> nil then
    ACopy.NextParagraphStyle := ATarget.ParagraphStyles[NextParagraphStyle.Copy(ATargetModel)];
end;

function TdxParagraphStyle.CopyFrom(ATargetModel: TdxCustomDocumentModel): TdxParagraphStyle;
begin
  Result := TdxParagraphStyle.Create(ATargetModel);
  CopyTo(Result);
end;

procedure TdxParagraphStyle.CopyProperties(ASource: TdxStyleBase);
var
  AParagraphStyle: TdxParagraphStyle absolute ASource;
begin
  Assert(ASource is TdxParagraphStyle);
  CharacterProperties.CopyFrom(AParagraphStyle.CharacterProperties.Info);
  ParagraphProperties.CopyFrom(AParagraphStyle.ParagraphProperties.Info);
end;

procedure TdxParagraphStyle.CopyTo(AStyle: TdxParagraphStyle);
var
  AMergedWithDefaultParagraphProperties, AStyleMergedWithDefaultParagraphProperties: TdxMergedParagraphProperties;
  AStyleMergedWithDefaultCharacterProperties, AMergedWithDefaultCharacterProperties: TdxMergedCharacterProperties;
begin
  AStyle.ParagraphProperties.CopyFrom(ParagraphProperties.Info);
  if FrameProperties <> nil then
  begin
    AStyle.CreateFrameProperties;
    AStyle.FrameProperties.CopyFrom(FrameProperties.Info);
  end;
  AStyle.CharacterProperties.CopyFrom(FCharacterProperties.Info);
  AStyle.AutoUpdate := AutoUpdate;
  AStyle.Tabs.CopyFrom(Tabs.Info);
  AStyle.StyleName := StyleName;
  if GetOwnNumberingListIndex >= 0 then
    AStyle.SetNumberingListIndex(TdxSimpleDocumentModel(DocumentModel).GetNumberingListIndex(AStyle.DocumentModel, GetOwnNumberingListIndex, NumberingListIndexMaxValue));

  if GetOwnListLevelIndex >= 0 then
    AStyle.SetNumberingListLevelIndex(GetOwnListLevelIndex);

  if Parent <> nil then
    AStyle.Parent := AStyle.Container.ParagraphStyles[Parent.Copy(AStyle.DocumentModel)];

  AStyleMergedWithDefaultParagraphProperties := AStyle.GetMergedWithDefaultParagraphProperties;
  try
    AMergedWithDefaultParagraphProperties := GetMergedWithDefaultParagraphProperties;
    try
      ParagraphProperties.ApplyPropertiesDiff(AStyle.ParagraphProperties,
        AStyleMergedWithDefaultParagraphProperties.Info,
        AMergedWithDefaultParagraphProperties.Info);
    finally
      AMergedWithDefaultParagraphProperties.Free;
    end;
  finally
    AStyleMergedWithDefaultParagraphProperties.Free;
  end;
  AStyleMergedWithDefaultCharacterProperties := AStyle.GetMergedWithDefaultCharacterProperties;
  try
    AMergedWithDefaultCharacterProperties := GetMergedWithDefaultCharacterProperties;
    try
      CharacterProperties.ApplyPropertiesDiff(AStyle.CharacterProperties,
        AStyleMergedWithDefaultCharacterProperties.Info,
        AMergedWithDefaultCharacterProperties.Info);
    finally
      AMergedWithDefaultCharacterProperties.Free;
    end;
  finally
    AStyleMergedWithDefaultCharacterProperties.Free;
  end;
end;

function TdxParagraphStyle.GetListLevelIndex: Integer;
begin
  if FNumberingListIndex >= 0 then
    Result := GetOwnListLevelIndex
  else
    if (FNumberingListIndex = NumberingListIndexNoNumberingList) or (Parent = nil) then
      Result := 0
    else
      if (FListLevelIndex > 0) and (Parent.GetNumberingListIndex >= NumberingListIndexMinValue) then
        Result := FListLevelIndex
      else
        Result := Parent.GetListLevelIndex;
end;


function TdxParagraphStyle.GetMergedCharacterProperties: TdxMergedCharacterProperties;
var
  AParentProperties: TdxMergedCharacterProperties;
begin
  Assert(not Deleted);
  Result := TdxMergedCharacterProperties.Create(CharacterProperties);
  if Parent <> nil then
  begin
    AParentProperties := Parent.GetMergedCharacterProperties;
    try
      Result.Merge(AParentProperties);
    finally
      AParentProperties.Free;
    end;
  end;
end;

function TdxParagraphStyle.GetMergedWithDefaultCharacterProperties: TdxMergedCharacterProperties;
begin
  Assert(not Deleted);
  Result := GetMergedCharacterProperties;
  Result.Merge(Container.DefaultCharacterProperties);
end;

function TdxParagraphStyle.GetNumberingListIndex: TdxNumberingListIndex;
begin
  if (FNumberingListIndex >= NumberingListIndexMinValue) or
    (FNumberingListIndex = NumberingListIndexNoNumberingList) or (Parent = nil) then
    Result := FNumberingListIndex
  else
    Result := Parent.GetNumberingListIndex;
end;

function TdxParagraphStyle.GetOwnListLevelIndex: Integer;
begin
  Result := FListLevelIndex;
end;

function TdxParagraphStyle.GetOwnNumberingListIndex: Integer;
begin
  Result := FNumberingListIndex;
end;

function TdxParagraphStyle.HasLinkedStyle: Boolean;
begin
  Result := Container.StyleLinkManager.HasLinkedCharacterStyle(Self);
end;

procedure TdxParagraphStyle.SetNumberingListIndex(ANumberingListIndex: TdxNumberingListIndex);
begin
  FNumberingListIndex := ANumberingListIndex;
end;

procedure TdxParagraphStyle.SetNumberingListLevelIndex(AListLevelIndex: Integer);
begin
  FListLevelIndex := AListLevelIndex;
end;

function TdxParagraphStyle.GetPieceTable: TdxCustomPieceTable;
begin
  Result := PieceTable;
end;

function TdxParagraphStyle.CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(PieceTable, CharacterProperties);
end;

function TdxParagraphStyle.CreateParagraphPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(PieceTable, ParagraphProperties);
end;

function TdxParagraphStyle.GetTabs: TdxTabFormattingInfo;
var
  ATabs, AParentTabs: TdxTabFormattingInfo;
begin
  if Parent = nil then
    Result := Tabs.GetTabs
  else
  begin
    ATabs := Tabs.GetTabs;
    try
      AParentTabs := Parent.GetTabs;
      try
        Result := TdxTabFormattingInfo.Merge(ATabs, AParentTabs);
      finally
        AParentTabs.Free;
      end;
    finally
      ATabs.Free;
    end;
  end;
end;

function TdxParagraphStyle.GetType: TdxStyleType;
begin
  Result := TdxStyleType.ParagraphStyle;
end;

procedure TdxParagraphStyle.MergePropertiesWithParent;
begin
  ParagraphProperties.Merge(Parent.ParagraphProperties);
end;

procedure TdxParagraphStyle.OnCharacterPropertiesChanged;
begin
  BeginUpdate;
  try
    NotifyStyleChanged;
    if HasLinkedStyle then
      LinkedStyle.CharacterProperties.CopyFrom(CharacterProperties);
  finally
    EndUpdate;
  end;
end;

procedure TdxParagraphStyle.OnObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
begin
  E.Start := 0;
  E.&End := MaxInt;
end;

procedure TdxParagraphStyle.OnParagraphPropertiesChanged;
begin
  BeginUpdate;
  try
    NotifyStyleChanged;
  finally
    EndUpdate;
  end;
end;

function TdxParagraphStyle.GetLinkedStyle: TdxCharacterStyle;
begin
  Result := Container.StyleLinkManager.GetLinkedCharacterStyle(Self);
end;

function TdxParagraphStyle.GetParent: TdxParagraphStyle;
begin
  Result := TdxParagraphStyle(inherited Parent);
end;

procedure TdxParagraphStyle.SetAutoUpdate(const Value: Boolean);
var
  AItem: TdxParagraphStyleChangeAutoUpdatePropertyHistoryItem;
begin
  if AutoUpdate = Value then
    Exit;
  AItem := TdxParagraphStyleChangeAutoUpdatePropertyHistoryItem.Create(Self);
  DocumentModel.History.Add(AItem);
  AItem.Execute;
end;

procedure TdxParagraphStyle.SetAutoUpdateCore(AAutoUpdate: Boolean);
begin
  FAutoUpdate := AAutoUpdate;
end;

procedure TdxParagraphStyle.SetNextParagraphStyle(Value: TdxParagraphStyle);
var
  AItem: TdxParagraphStyleChangeNextParagraphStylePropertyHistoryItem;
begin
  if NextParagraphStyle = Value then
   	Exit;
  AItem := TdxParagraphStyleChangeNextParagraphStylePropertyHistoryItem.Create(Self, NextParagraphStyle, Value);
  DocumentModel.History.Add(AItem);
  AItem.Execute;
  FNextParagraphStyle := Value;
end;

procedure TdxParagraphStyle.SetNextParagraphStyleCore(Value: TdxParagraphStyle);
begin
  FNextParagraphStyle := Value;
end;

procedure TdxParagraphStyle.SetParent(const Value: TdxParagraphStyle);
begin
  inherited Parent := Value;
end;

procedure TdxParagraphStyle.SubscribeCharacterPropertiesEvents;
begin
  CharacterProperties.OnObtainAffectedRange.Add(OnObtainAffectedRange);
end;

procedure TdxParagraphStyle.SubscribeParagraphPropertiesEvents;
begin
  ParagraphProperties.OnObtainAffectedRange.Add(OnObtainAffectedRange);
end;

{ TdxParagraphStyleCollection }

function TdxParagraphStyleCollection.CreateTocStyle(ALevel: Integer): Integer;
var
  AStyle: TdxParagraphStyle;
begin
  AStyle := TdxParagraphStyle.Create(DocumentModel, DefaultItem, GetTocStyleName(ALevel));
  Result := Add(AStyle);
  SetupTocStyle(AStyle, ALevel);
end;

function TdxParagraphStyleCollection.GetHeadingParagraphStyle(ALevel: Integer): Integer;
var
  AParagraphStyleIndex, ACharacterStyleIndex: Integer;
  AParagraphStyle: TdxParagraphStyle;
  ACharacterStyle: TdxCharacterStyle;
  AContainer: IdxStylesContainer;
begin
  AParagraphStyleIndex := GetHeadingParagraphStyleCore(ALevel);
  AContainer := DocumentModel as IdxStylesContainer;
  ACharacterStyleIndex := AContainer.CharacterStyles.GetHeadingCharacterStyleCore(ALevel);
  AParagraphStyle := Items[AParagraphStyleIndex];
  ACharacterStyle := AContainer.CharacterStyles[ACharacterStyleIndex];
  if not AParagraphStyle.HasLinkedStyle and not ACharacterStyle.HasLinkedStyle then
    AContainer.StyleLinkManager.CreateLink(AParagraphStyle, ACharacterStyle);
  Result := AParagraphStyleIndex;
end;

function TdxParagraphStyleCollection.GetTocStyle(ALevel: Integer): Integer;
begin
  Result := GetStyleIndexByName(GetTocStyleName(ALevel), True);
end;

function TdxParagraphStyleCollection.CreateDefaultItem: TdxStyleBase;
begin
  Result := TdxParagraphStyle.Create(DocumentModel, nil, DefaultParagraphStyleName);
end;

function TdxParagraphStyleCollection.CreateHeadingParagraphStyle(const AStyleName: string; ALevel: Integer): Integer;
var
  AStyle: TdxParagraphStyle;
begin
  AStyle := TdxParagraphStyle.Create(DocumentModel, DefaultItem, AStyleName);
  SetupHeadingParagraphStyle(AStyle, ALevel);
  AStyle.NextParagraphStyle := DefaultItem;
  Result := Add(AStyle);
end;

function TdxParagraphStyleCollection.GetItem(Index: Integer): TdxParagraphStyle;
begin
  Result := TdxParagraphStyle(inherited Items[Index]);
end;

function TdxParagraphStyleCollection.GetDefaultItem: TdxParagraphStyle;
begin
  Result := TdxParagraphStyle(inherited DefaultItem);
end;

function TdxParagraphStyleCollection.GetHeadingParagraphStyleCore(ALevel: Integer): Integer;
var
  AStyleName: string;
  AIndex: Integer;
begin
  AStyleName := Format('Heading %d', [ALevel]);
  AIndex := GetStyleIndexByName(AStyleName);
  if AIndex >= 0 then
    Exit(AIndex);
  Result := CreateHeadingParagraphStyle(AStyleName, ALevel);
end;

function TdxParagraphStyleCollection.GetTocStyleName(ALevel: Integer): string;
begin
  Result := Format('TOC %d', [ALevel]);
end;

procedure TdxParagraphStyleCollection.SetupHeadingParagraphStyle(AStyle: TdxParagraphStyle; ALevel: Integer);
var
  ACharacterProperties: TdxCharacterProperties;
  AUnitConverter: TdxDocumentModelUnitConverter;
begin
  AUnitConverter := DocumentModel.UnitConverter;
  AStyle.ParagraphProperties.OutlineLevel := ALevel;
  AStyle.ParagraphProperties.SpacingAfter := 0;
  if ALevel = 1 then
    AStyle.ParagraphProperties.SpacingBefore := AUnitConverter.PointsToModelUnits(24)
  else
    AStyle.ParagraphProperties.SpacingBefore := AUnitConverter.PointsToModelUnits(10);
  ACharacterProperties := AStyle.CharacterProperties;
  case ALevel of
    1:
      begin
        ACharacterProperties.DoubleFontSize := 28;
        ACharacterProperties.FontBold := True;
        ACharacterProperties.ForeColor := TdxAlphaColors.FromArgb($36, $5F, $91);
      end;
    2:
      begin
        ACharacterProperties.DoubleFontSize := 26;
        ACharacterProperties.FontBold := True;
        ACharacterProperties.ForeColor := TdxAlphaColors.FromArgb($4F, $81, $BD);
      end;
    3:
      begin
        ACharacterProperties.FontBold := True;
        ACharacterProperties.ForeColor := TdxAlphaColors.FromArgb($4F, $81, $BD);
      end;
    4:
      begin
        ACharacterProperties.FontBold := True;
        ACharacterProperties.FontItalic := True;
        ACharacterProperties.ForeColor := TdxAlphaColors.FromArgb($4F, $81, $BD);
      end;
    5:
      ACharacterProperties.ForeColor := TdxAlphaColors.FromArgb($24, $3F, $60);
    6:
      begin
        ACharacterProperties.FontItalic := True;
        ACharacterProperties.ForeColor := TdxAlphaColors.FromArgb($24, $3F, $60);
      end;
    7:
      begin
        ACharacterProperties.FontItalic := True;
        ACharacterProperties.ForeColor := TdxAlphaColors.FromArgb($40, $40, $40);
      end;
    8:
      begin
        ACharacterProperties.DoubleFontSize := 20;
        ACharacterProperties.ForeColor := TdxAlphaColors.FromArgb($40, $40, $40);
      end;
    9:
      begin
        ACharacterProperties.DoubleFontSize := 20;
        ACharacterProperties.FontItalic := True;
        ACharacterProperties.ForeColor := TdxAlphaColors.FromArgb($40, $40, $40);
      end;
  end;
end;

procedure TdxParagraphStyleCollection.SetupTocStyle(AStyle: TdxParagraphStyle; ALevel: Integer);
begin
  AStyle.ParagraphProperties.OutlineLevel := 0;
  AStyle.ParagraphProperties.SpacingAfter := DocumentModel.UnitConverter.PointsToModelUnits(5);
  AStyle.ParagraphProperties.LeftIndent := 221 * (ALevel - 1);
end;

procedure TdxParagraphStyleCollection.NotifyDocumentStyleDeleting(AParagraphStyle: TdxStyleBase);
var
  I: Integer;
  AStyle: TdxParagraphStyle absolute AParagraphStyle;
begin
  inherited NotifyDocumentStyleDeleting(AStyle);
  for I := 0 to Count - 1 do
    if Self[I].NextParagraphStyle = AStyle then
      Self[i].NextParagraphStyle := nil;
end;

procedure TdxParagraphStyleCollection.NotifyPieceTableStyleDeleting(APieceTable: TdxCustomPieceTable; AParagraphStyle: TdxStyleBase);
var
  I: Integer;
  AParagraphs: TdxSimpleParagraphCollection;
  AStyle: TdxParagraphStyle absolute AParagraphStyle;
begin
  AParagraphs := TdxSimpleParagraphCollection(APieceTable.Paragraphs);
  for I := 0 to AParagraphs.Count - 1 do
    if AParagraphs[I].ParagraphStyle = AStyle then
      AParagraphs[I].ParagraphStyleIndex := DefaultItemIndex;
end;

{ TdxCharacterStyle }

constructor TdxCharacterStyle.Create(ADocumentModel: TdxCustomDocumentModel;
  AParentStyle: TdxStyleBase = nil; const AStyleName: string = '');
begin
  inherited Create(ADocumentModel, AParentStyle, AStyleName);
  FContainer := ADocumentModel as IdxStylesContainer;
  FCharacterProperties := TdxCharacterProperties.Create(Self);
end;

destructor TdxCharacterStyle.Destroy;
begin
  FreeAndNil(FCharacterProperties);
  inherited Destroy;
end;

function TdxCharacterStyle.CalculateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxParagraphFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFormattingChangeType.ParagraphStyle);
end;

function TdxCharacterStyle.Copy(ATargetModel: TdxCustomDocumentModel): Integer;
var
  ATargetStyles: TdxCharacterStyleCollection;
  I: Integer;
  AStyle: TdxCharacterStyle;
  ATarget: IdxStylesContainer;
begin
  ATarget := ATargetModel as IdxStylesContainer;
  ATargetStyles := ATarget.CharacterStyles;
  for I := 0 to ATargetStyles.Count - 1 do
    if StyleName = ATargetStyles[I].StyleName then
      Exit(I);

  AStyle := CopyFrom(ATargetModel);
  Result := ATargetStyles.AddNewStyle(AStyle);
end;

procedure TdxCharacterStyle.CopyProperties(ASource: TdxStyleBase);
begin
  Assert(ASource is TdxCharacterStyle);
  CharacterProperties.CopyFrom(TdxCharacterStyle(ASource).CharacterProperties.Info);
end;

function TdxCharacterStyle.GetMergedCharacterProperties: TdxMergedCharacterProperties;
var
  AParentProperties: TdxMergedCharacterProperties;
begin
  Assert(not Deleted);
  Result := TdxMergedCharacterProperties.Create(CharacterProperties);
  if Parent <> nil then
  begin
    AParentProperties := Parent.GetMergedCharacterProperties;
    try
      Result.Merge(AParentProperties);
    finally
      AParentProperties.Free;
    end;
  end;
end;

function TdxCharacterStyle.GetMergedWithDefaultCharacterProperties: TdxMergedCharacterProperties;
begin
  Assert(not Deleted);
  Result := GetMergedCharacterProperties;
  Result.Merge(Container.DefaultCharacterProperties);
end;

function TdxCharacterStyle.GetParent: TdxCharacterStyle;
begin
  Result := TdxCharacterStyle(inherited Parent);
end;

function TdxCharacterStyle.GetPieceTable: TdxCustomPieceTable;
begin
  Result := PieceTable;
end;

function TdxCharacterStyle.CopyFrom(ATargetModel: TdxCustomDocumentModel): TdxCharacterStyle;
begin
  Result := TdxCharacterStyle.Create(ATargetModel, nil);
  CopyTo(Result);
end;

procedure TdxCharacterStyle.CopyTo(AStyle: TdxCharacterStyle);
var
  AStyleMergedWithDefaultCharacterProperties, AMergedWithDefaultCharacterProperties: TdxMergedCharacterProperties;
begin
  AStyle.CharacterProperties.CopyFrom(CharacterProperties.Info);
  AStyle.StyleName := StyleName;
  if Parent <> nil then
    AStyle.Parent := AStyle.Container.CharacterStyles[Parent.Copy(AStyle.DocumentModel)];
  AStyleMergedWithDefaultCharacterProperties := AStyle.GetMergedWithDefaultCharacterProperties;
  try
    AMergedWithDefaultCharacterProperties := GetMergedWithDefaultCharacterProperties;
    try
      CharacterProperties.ApplyPropertiesDiff(AStyle.CharacterProperties,
        AStyleMergedWithDefaultCharacterProperties.Info, AMergedWithDefaultCharacterProperties.Info);
    finally
      AMergedWithDefaultCharacterProperties.Free;
    end;
  finally
    AStyleMergedWithDefaultCharacterProperties.Free;
  end;
end;

function TdxCharacterStyle.CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(PieceTable, CharacterProperties);
end;
function TdxCharacterStyle.GetType: TdxStyleType;
begin
  Result := TdxStyleType.CharacterStyle;
end;

procedure TdxCharacterStyle.MergePropertiesWithParent;
begin
  Assert(Parent <> nil);
  CharacterProperties.Merge(Parent.CharacterProperties);
end;

procedure TdxCharacterStyle.OnCharacterPropertiesChanged;
begin
  OnCharacterPropertiesChangedCore;
end;

procedure TdxCharacterStyle.OnCharacterPropertiesChangedCore;
begin
  BeginUpdate;
  try
    NotifyStyleChanged;
    if HasLinkedStyle then
      LinkedStyle.CharacterProperties.CopyFrom(CharacterProperties);
  finally
    EndUpdate;
  end;
end;

procedure TdxCharacterStyle.SetParent(const Value: TdxCharacterStyle);
begin
  inherited Parent := Value;
end;

function TdxCharacterStyle.GetHasLinkedStyle: Boolean;
begin
  Result := Container.StyleLinkManager.HasLinkedParagraphStyle(Self);
end;

function TdxCharacterStyle.GetLinkedStyle: TdxParagraphStyle;
begin
  Result := Container.StyleLinkManager.GetLinkedParagraphStyle(Self);
end;

{ TdxCharacterStyleCollection }

constructor TdxCharacterStyleCollection.Create(
  ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel);
  InnerList.Add(CreateLineNumberingStyle);
  InnerList.Add(CreateHyperlinkStyle);
end;

function TdxCharacterStyleCollection.CreateDefaultItem: TdxStyleBase;
begin
  Result := TdxCharacterStyle.Create(DocumentModel, nil, DefaultCharacterStyleName);
  Result.SemihiddenCore := True;
end;

function TdxCharacterStyleCollection.CreateHyperlinkStyle: TdxCharacterStyle;
begin
  Result := TdxCharacterStyle.Create(DocumentModel, nil, HyperlinkStyleName);
  Result.CharacterProperties.BeginInit;
  try
    Result.CharacterProperties.ForeColor := TdxAlphaColors.Blue;
    Result.CharacterProperties.FontUnderlineType := TdxUnderlineType.Single;
  finally
    Result.CharacterProperties.EndInit;
  end;
end;

function TdxCharacterStyleCollection.CreateLineNumberingStyle: TdxCharacterStyle;
begin
  Result := TdxCharacterStyle.Create(DocumentModel, Items[EmptyCharacterStyleIndex], LineNumberingStyleName);
  Result.SemihiddenCore := True;
end;

function TdxCharacterStyleCollection.CreateHeadingCharacterStyle(const AStyleName: string; ALevel: Integer): Integer;
var
  AStyle: TdxCharacterStyle;
begin
  AStyle := TdxCharacterStyle.Create(DocumentModel, DefaultItem, AStyleName);
  SetupHeadingCharacterStyle(AStyle, ALevel);
  Result := Add(AStyle);
end;

function TdxCharacterStyleCollection.GetHeadingCharacterStyleCore(ALevel: Integer): Integer;
var
  AStyleName: string;
  AIndex: Integer;
begin
  AStyleName := Format('Heading %d Char', [ALevel]);
  AIndex := GetStyleIndexByName(AStyleName);
  if AIndex >= 0 then
    Exit(AIndex);
  Result := CreateHeadingCharacterStyle(AStyleName, ALevel);
end;

procedure TdxCharacterStyleCollection.SetupHeadingCharacterStyle(AStyle: TdxCharacterStyle; ALevel: Integer);
begin
  AStyle.Hidden := True;
end;

function TdxCharacterStyleCollection.GetItem(Index: Integer): TdxCharacterStyle;
begin
  Result := TdxCharacterStyle(inherited Items[Index]);
end;

function TdxCharacterStyleCollection.GetDefaultItem: TdxCharacterStyle;
begin
  Result := TdxCharacterStyle(inherited DefaultItem);
end;

procedure TdxCharacterStyleCollection.NotifyPieceTableStyleDeleting(
  APieceTable: TdxCustomPieceTable; AStyle: TdxStyleBase);
var
  ARuns: TdxTextRunCollection;
  I, ACount: TdxRunIndex;
begin
  ARuns := TdxSimplePieceTable(APieceTable).Runs;
  ACount := ARuns.Count;
  for I := 0 to ACount - 1 do
  begin
    if ARuns[I].CharacterStyle = AStyle then
      ARuns[i].CharacterStyleIndex := DefaultItemIndex;
  end;
end;

{ TdxStyleLinkManager }

constructor TdxStyleLinkManager.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
  FParagraphStyleToCharacterStyleLinks := TDictionary<TdxParagraphStyle, TdxCharacterStyle>.Create;
  FCharacterStyleToParagraphStyleLinks := TDictionary<TdxCharacterStyle, TdxParagraphStyle>.Create;
end;

destructor TdxStyleLinkManager.Destroy;
begin
  FreeAndNil(FParagraphStyleToCharacterStyleLinks);
  FreeAndNil(FCharacterStyleToParagraphStyleLinks);
  inherited Destroy;
end;

procedure TdxStyleLinkManager.CreateLink(AParagraphStyle: TdxParagraphStyle; ACharacterStyle: TdxCharacterStyle);
var
  ATransaction: TdxHistoryTransaction;
  AItem: TdxCreateStyleLinkHistoryItem;
begin
  if AParagraphStyle.HasLinkedStyle or ACharacterStyle.HasLinkedStyle then
    Exit;

  if AParagraphStyle.Deleted or ACharacterStyle.Deleted then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionErrorLinkDeletedStyle));

  DocumentModel.BeginUpdate;
  try
    ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
    try
      ACharacterStyle.CharacterProperties.CopyFrom(AParagraphStyle.CharacterProperties);
      AItem := TdxCreateStyleLinkHistoryItem.Create(AParagraphStyle, ACharacterStyle);
      DocumentModel.History.Add(AItem);
      AItem.Execute;
    finally
      ATransaction.Free;
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxStyleLinkManager.CreateLinkCore(AParagraphStyle: TdxParagraphStyle; ACharacterStyle: TdxCharacterStyle);
begin
  if not FParagraphStyleToCharacterStyleLinks.ContainsKey(AParagraphStyle) then
    FParagraphStyleToCharacterStyleLinks.Add(AParagraphStyle, ACharacterStyle)
  else
    FParagraphStyleToCharacterStyleLinks[AParagraphStyle] := ACharacterStyle;

  if not FCharacterStyleToParagraphStyleLinks.ContainsKey(ACharacterStyle) then
    FCharacterStyleToParagraphStyleLinks.Add(ACharacterStyle, AParagraphStyle)
  else
    FCharacterStyleToParagraphStyleLinks[ACharacterStyle] := AParagraphStyle;
end;

procedure TdxStyleLinkManager.DeleteLinkCore(AParagraphStyle: TdxParagraphStyle; ACharacterStyle: TdxCharacterStyle);
begin
  FParagraphStyleToCharacterStyleLinks.Remove(AParagraphStyle);
  FCharacterStyleToParagraphStyleLinks.Remove(ACharacterStyle);
end;

procedure TdxStyleLinkManager.DeleteLink(ACharacterStyle: TdxCharacterStyle);
begin
  if not ACharacterStyle.HasLinkedStyle then
    TdxRichEditExceptions.ThrowArgumentException('CharacterStyle', ACharacterStyle);
  DeleteLinkCore(ACharacterStyle.LinkedStyle, ACharacterStyle);
end;

procedure TdxStyleLinkManager.DeleteLink(AParagraphStyle: TdxParagraphStyle);
begin
  if not AParagraphStyle.HasLinkedStyle then
    TdxRichEditExceptions.ThrowArgumentException('ParagraphStyle', AParagraphStyle);
  DeleteLinkCore(AParagraphStyle, AParagraphStyle.LinkedStyle);
end;

function TdxStyleLinkManager.GetLinkedParagraphStyle(ACharacterStyle: TdxCharacterStyle): TdxParagraphStyle;
begin
  if not FCharacterStyleToParagraphStyleLinks.TryGetValue(ACharacterStyle, Result) then
    Result := nil;
end;

function TdxStyleLinkManager.GetLinkedCharacterStyle(AParagraphStyle: TdxParagraphStyle): TdxCharacterStyle;
begin
  if not FParagraphStyleToCharacterStyleLinks.TryGetValue(AParagraphStyle, Result) then
    Result := nil;
end;

function TdxStyleLinkManager.HasLinkedParagraphStyle(ACharacterStyle: TdxCharacterStyle): Boolean;
begin
  Result := FCharacterStyleToParagraphStyleLinks.ContainsKey(ACharacterStyle);
end;

function TdxStyleLinkManager.HasLinkedCharacterStyle(AParagraphStyle: TdxParagraphStyle): Boolean;
begin
  Result := FParagraphStyleToCharacterStyleLinks.ContainsKey(AParagraphStyle);
end;

{ TdxStyleTopologicalComparer<T> }

function TdxStyleTopologicalComparer<T>.Compare(const Left, Right: T): Integer;
begin
  if Left.Parent = TdxStyleBase(Right) then
    Result := 1
  else
    if Right.Parent = TdxStyleBase(Left) then
      Result := -1
    else
      Result := 0;
end;

end.
