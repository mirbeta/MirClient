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

unit dxRichEdit.DocumentModel.TabFormatting;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Classes, Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,

  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Core;

type
  TdxTabAlignmentType = TdxRichEditTabAlignmentType;
  TdxTabLeaderType = TdxRichEditTabLeaderType;

  { TdxTabInfo }

  TdxTabInfo = packed record
  public const
    DefaultAlignment = TdxTabAlignmentType.Left;
    DefaultLeader = TdxTabLeaderType.None;
  private
    FAlignment: TdxTabAlignmentType;
    FDeleted: Boolean;
    FLeader: TdxTabLeaderType;
    FPosition: Integer;
    FIsDefault: Boolean;
  public
    class function Create(APosition: Integer; AAlignment: TdxTabAlignmentType; ALeader: TdxTabLeaderType; ADeleted, AIsDefault: Boolean): TdxTabInfo; overload; static;

    class function Create(APosition: Integer): TdxTabInfo; overload; static;
    class function Create(APosition: Integer; AAlignment: TdxTabAlignmentType): TdxTabInfo; overload; static;
    class function Create(APosition: Integer; ALeader: TdxTabLeaderType): TdxTabInfo; overload; static;

    class function CreateDefault(APosition: Integer): TdxTabInfo; overload; static;

    class operator Equal(const A, B: TdxTabInfo): Boolean;
    class operator NotEqual(const A, B: TdxTabInfo): Boolean;

    function GetLayoutPosition(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter): Integer;

    property Alignment: TdxTabAlignmentType read FAlignment;
    property Deleted: Boolean read FDeleted;
    property Leader: TdxTabLeaderType read FLeader;
    property IsDefault: Boolean read FIsDefault;
    property Position: Integer read FPosition;
  end;

  { TdxTabInfoComparer }

  TdxTabInfoComparer = class(TcxIUnknownObject, IComparer<TdxTabInfo>)
  public
    function Compare(const X, Y: TdxTabInfo): Integer;
  end;

  { TdxTabFormattingInfo }

  TdxTabFormattingInfo = class(TdxCloneableIUnknownObject)
  strict private
    class var FComparer: TdxTabInfoComparer;
    class constructor Initialize;
    class destructor Finalize;
  private
    FTabInfos: TList<TdxTabInfo>;
    function GetItem(Index: Integer): TdxTabInfo;
    procedure SetItem(Index: Integer; const Value: TdxTabInfo);
    function GetCount: Integer;
  public
    constructor Create; overload; override;
    constructor Create(ASource: TList<TdxTabInfo>); reintroduce; overload;
    destructor Destroy; override;

    function Add(const AItem: TdxTabInfo): Integer;
    function IndexOf(const AItem: TdxTabInfo): Integer;
    function Contains(const AItem: TdxTabInfo): Boolean;
    procedure Remove(const AItem: TdxTabInfo);
    procedure Clear;
    procedure AddRange(AFrom: TdxTabFormattingInfo); overload;
    procedure AddRange(AFrom: TArray<TdxTabInfo>); overload;
    function FindTabItemIndexByPosition(APosition: Integer): Integer;

    function FindNextTab(APos: Integer): TdxNullableValue<TdxTabInfo>;
    function Clone: TdxTabFormattingInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Equals(Obj: TObject): Boolean; override;
    class function Merge(AMaster: TdxTabFormattingInfo; ASlave: TdxTabFormattingInfo): TdxTabFormattingInfo; static;
    class procedure MergeCore(AMaster, ASlave, AResult: TdxTabFormattingInfo); static;
    class function NormalizeTabs(ATabs: TdxTabFormattingInfo; AParentTabs: TdxTabFormattingInfo): TdxTabFormattingInfo; static;
    procedure OnChanged; virtual;

    property List: TList<TdxTabInfo> read FTabInfos;
    property Items[Index: Integer]: TdxTabInfo read GetItem write SetItem; default;
    property Count: Integer read GetCount;
  end;

  { TdxTabFormattingInfoWithoutSort }

  TdxTabFormattingInfoWithoutSort = class(TdxTabFormattingInfo, IdxBatchUpdateable, IdxBatchUpdateHandler)
  private
    FBatchUpdateHelper: TdxBatchUpdateHelper;
    function GetIsUpdateLocked: Boolean;
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;
    procedure OnFirstBeginUpdate;
    procedure OnBeginUpdate;
    procedure OnEndUpdate;
    procedure OnLastEndUpdate;
    procedure OnCancelUpdate;
    procedure OnLastCancelUpdate;
    procedure OnChanged; override;

    property BatchUpdateHelper: TdxBatchUpdateHelper read FBatchUpdateHelper;
    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
  end;

  { TdxTabFormattingInfoCache }

  TdxTabFormattingInfoCache = class(TdxUniqueItemsCache<TdxTabFormattingInfo>)
  public const
    EmptyTabFormattingOptionIndex = 0;
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTabFormattingInfo; override;
  end;

  { TdxTabProperties }

  TdxTabProperties = class(TdxRichEditIndexBasedObject<TdxTabFormattingInfo>)
  private
    FOwner: IdxParagraphPropertiesContainer;
    function GetItem(Index: Integer): TdxTabInfo;
    procedure SetItem(Index: Integer; const Value: TdxTabInfo);
    function SuppressInsertTabs: Boolean;
  protected
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
  public
    constructor Create(const AOwner: IdxParagraphPropertiesContainer); reintroduce;

    procedure SetTabs(ATabs: TdxTabFormattingInfo);
    function GetTabs: TdxTabFormattingInfo;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTabFormattingInfo>; override;
    procedure SetTabCore(AIndex: Integer; const AValue: TdxTabInfo);
    function GetObtainAffectedRangeListener: IdxObtainAffectedRangeListener; override;

    property Items[Index: Integer]: TdxTabInfo read GetItem write SetItem; default;
  end;

implementation

uses
  RTLConsts,
  dxRichEdit.DocumentModel.Cache.Simple;

{ TdxTabInfo }

class function TdxTabInfo.Create(APosition: Integer): TdxTabInfo;
begin
  Result := Create(APosition, DefaultAlignment, DefaultLeader, False, False);
end;

class function TdxTabInfo.Create(APosition: Integer; AAlignment: TdxTabAlignmentType): TdxTabInfo;
begin
  Result := Create(APosition, AAlignment, DefaultLeader, False, False);
end;

class function TdxTabInfo.Create(APosition: Integer; ALeader: TdxTabLeaderType): TdxTabInfo;
begin
  Result := Create(APosition, DefaultAlignment, ALeader, False, False);
end;

class function TdxTabInfo.Create(APosition: Integer; AAlignment: TdxTabAlignmentType; ALeader: TdxTabLeaderType;
  ADeleted, AIsDefault: Boolean): TdxTabInfo;
begin
  Result.FPosition := APosition;
  Result.FAlignment := AAlignment;
  Result.FLeader := ALeader;
  Result.FDeleted := ADeleted;
  Result.FIsDefault := AIsDefault;
end;

class function TdxTabInfo.CreateDefault(APosition: Integer): TdxTabInfo;
begin
  Result := Create(APosition, DefaultAlignment, DefaultLeader, False, True);
end;

class operator TdxTabInfo.Equal(const A, B: TdxTabInfo): Boolean;
begin
  Result := (A.Position = B.Position) and
    (A.Alignment = B.Alignment) and
    (A.Leader = B.Leader) and
    (A.Deleted = B.Deleted);
end;

class operator TdxTabInfo.NotEqual(const A, B: TdxTabInfo): Boolean;
begin
  Result := not (A = B);
end;

function TdxTabInfo.GetLayoutPosition(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter): Integer;
begin
  Result := AUnitConverter.ToLayoutUnits(Position);
end;

{ TdxTabFormattingInfo }

constructor TdxTabFormattingInfo.Create;
begin
  inherited Create;
  FTabInfos := TList<TdxTabInfo>.Create;
end;

constructor TdxTabFormattingInfo.Create(ASource: TList<TdxTabInfo>);
begin
  inherited Create;
  FTabInfos := TList<TdxTabInfo>.Create(ASource);
end;

destructor TdxTabFormattingInfo.Destroy;
begin
  FreeAndNil(FTabInfos);
  inherited Destroy;
end;

function TdxTabFormattingInfo.Add(const AItem: TdxTabInfo): Integer;
var
  AIndex: Integer;
begin
  AIndex := FindTabItemIndexByPosition(AItem.Position);
  if AIndex >= 0 then
  begin
    Items[AIndex] := AItem;
    Result := AIndex;
  end
  else
  begin
    FTabInfos.Add(AItem);
    OnChanged;
    Result := Count - 1;
  end;
end;

procedure TdxTabFormattingInfo.AddRange(AFrom: TdxTabFormattingInfo);
var
  I: Integer;
begin
  for I := 0 to AFrom.Count - 1 do
    Add(AFrom[I]);
  OnChanged;
end;

procedure TdxTabFormattingInfo.AddRange(AFrom: TArray<TdxTabInfo>);
var
  I: Integer;
begin
  for I := 0 to High(AFrom) do
    Add(AFrom[I]);
  OnChanged;
end;

procedure TdxTabFormattingInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxTabFormattingInfo absolute Source;
var
  I: Integer;
begin
  FTabInfos.Clear;
  for I := 0 to AInfo.FTabInfos.Count - 1 do
    FTabInfos.Add(AInfo.FTabInfos[I]);
end;

function TdxTabFormattingInfo.Equals(Obj: TObject): Boolean;
var
  AInfo: TdxTabFormattingInfo absolute Obj;
  I: Integer;
begin
  if Count <> AInfo.Count then
    Exit(False);
  for I := 0 to Count - 1 do
    if Items[I] <> AInfo[I] then
      Exit(False);
  Result := True;
end;

procedure TdxTabFormattingInfo.Clear;
begin
  FTabInfos.Clear;
end;

function TdxTabFormattingInfo.Clone: TdxTabFormattingInfo;
begin
  Result := TdxTabFormattingInfo(inherited Clone);
end;

function TdxTabFormattingInfo.Contains(const AItem: TdxTabInfo): Boolean;
begin
  Result := FTabInfos.Contains(AItem);
end;

class destructor TdxTabFormattingInfo.Finalize;
begin
  FComparer.Free;
end;

function TdxTabFormattingInfo.FindNextTab(APos: Integer): TdxNullableValue<TdxTabInfo>;
var
  I: Integer;
begin
  Result.Reset;
  for I := 0 to Count - 1 do
    if Items[I].Position > APos then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TdxTabFormattingInfo.FindTabItemIndexByPosition(APosition: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].Position = APosition then
    begin
      Result := I;
      Break;
    end;
end;

function TdxTabFormattingInfo.GetCount: Integer;
begin
  Result := FTabInfos.Count;
end;

function TdxTabFormattingInfo.GetItem(Index: Integer): TdxTabInfo;
begin
  Result := FTabInfos[Index];
end;

function TdxTabFormattingInfo.IndexOf(const AItem: TdxTabInfo): Integer;
begin
  Result := FTabInfos.IndexOf(AItem);
end;

class constructor TdxTabFormattingInfo.Initialize;
begin
  FComparer := TdxTabInfoComparer.Create;
end;

class function TdxTabFormattingInfo.Merge(AMaster, ASlave: TdxTabFormattingInfo): TdxTabFormattingInfo;
var
  ATabFormattingInfo: TdxTabFormattingInfoWithoutSort;
begin
  ATabFormattingInfo := TdxTabFormattingInfoWithoutSort.Create;
  ATabFormattingInfo.BeginUpdate;
  try
    MergeCore(AMaster, ASlave, ATabFormattingInfo);
  finally
    ATabFormattingInfo.EndUpdate;
  end;
  Result := ATabFormattingInfo;
end;

class procedure TdxTabFormattingInfo.MergeCore(AMaster, ASlave, AResult: TdxTabFormattingInfo);
var
  I: Integer;
begin
  for I := 0 to ASlave.Count - 1 do
    if not ASlave[I].Deleted then
      AResult.Add(ASlave[I]);
  for I := 0 to AMaster.Count - 1 do
    AResult.Add(AMaster[I]);
  for I := AResult.Count - 1 downto 0 do
    if AResult[I].Deleted then
      AResult.FTabInfos.Delete(I);
end;

class function TdxTabFormattingInfo.NormalizeTabs(ATabs: TdxTabFormattingInfo; AParentTabs: TdxTabFormattingInfo): TdxTabFormattingInfo;
var
  AParentTabCount, ACount, AParentTabIndex, ATabIndex: Integer;
  ATabInfos: TList<TdxTabInfo>;
  ATab, AParentTab: TdxTabInfo;
begin
  AParentTabCount := AParentTabs.Count;
  if AParentTabCount = 0 then
    Exit(nil);
  ATabInfos := TList<TdxTabInfo>.Create;
  try
    ATabInfos.Capacity := ATabs.Count;
    ACount := ATabs.Count;
    AParentTabIndex := 0;
    ATabIndex := 0;
    while (ATabIndex < ACount) and (AParentTabIndex < AParentTabCount) do
    begin
      ATab := ATabs[ATabIndex];
      AParentTab := AParentTabs[AParentTabIndex];
      Assert(not ATab.Deleted);
      if ATab.Position = AParentTab.Position then
      begin
        if ATab <> AParentTab then
          ATabInfos.Add(ATab);
        Inc(AParentTabIndex);
        Inc(ATabIndex);
      end
      else
        if ATab.Position < AParentTab.Position then
        begin
          ATabInfos.Add(ATab);
          Inc(ATabIndex);
        end
        else
        begin
          if not AParentTab.Deleted then
            ATabInfos.Add(TdxTabInfo.Create(AParentTab.Position, AParentTab.Alignment, AParentTab.Leader, True, False));
          Inc(AParentTabIndex);
        end;
    end;
    while ATabIndex < ACount do
    begin
      Assert(not ATabs[ATabIndex].Deleted);
      ATabInfos.Add(ATabs[ATabIndex]);
      Inc(ATabIndex);
    end;
    while AParentTabIndex < AParentTabCount do
    begin
      AParentTab := AParentTabs[AParentTabIndex];
      if not AParentTab.Deleted then
        ATabInfos.Add(TdxTabInfo.Create(AParentTab.Position, AParentTab.Alignment, AParentTab.Leader, True, False));
      Inc(AParentTabIndex);
    end;
    Result := TdxTabFormattingInfo.Create(ATabInfos);
  finally
    ATabInfos.Free;
  end;
end;

procedure TdxTabFormattingInfo.OnChanged;
begin
  FTabInfos.Sort(FComparer);
end;

procedure TdxTabFormattingInfo.Remove(const AItem: TdxTabInfo);
var
  AIndex: Integer;
begin
  AIndex := IndexOf(AItem);
  if AIndex >= 0 then
    FTabInfos.Delete(AIndex);
end;

procedure TdxTabFormattingInfo.SetItem(Index: Integer; const Value: TdxTabInfo);
begin
  FTabInfos[Index] := Value;
  OnChanged;
end;

{ TdxTabFormattingInfoWithoutSort }

constructor TdxTabFormattingInfoWithoutSort.Create;
begin
  inherited Create;
  FBatchUpdateHelper := TdxBatchUpdateHelper.Create(Self);
end;

destructor TdxTabFormattingInfoWithoutSort.Destroy;
begin
  FreeAndNil(FBatchUpdateHelper);
  inherited Destroy;
end;

procedure TdxTabFormattingInfoWithoutSort.BeginUpdate;
begin
  FBatchUpdateHelper.BeginUpdate;
end;

procedure TdxTabFormattingInfoWithoutSort.CancelUpdate;
begin
  FBatchUpdateHelper.CancelUpdate;
end;

procedure TdxTabFormattingInfoWithoutSort.EndUpdate;
begin
  FBatchUpdateHelper.EndUpdate;
end;

function TdxTabFormattingInfoWithoutSort.GetBatchUpdateHelper: TdxBatchUpdateHelper;
begin
  Result := FBatchUpdateHelper;
end;

function TdxTabFormattingInfoWithoutSort.GetIsUpdateLocked: Boolean;
begin
  Result := FBatchUpdateHelper.IsUpdateLocked;
end;

procedure TdxTabFormattingInfoWithoutSort.OnBeginUpdate;
begin
  // do nothing
end;

procedure TdxTabFormattingInfoWithoutSort.OnCancelUpdate;
begin
  // do nothing
end;

procedure TdxTabFormattingInfoWithoutSort.OnChanged;
begin
  if not IsUpdateLocked then
    inherited OnChanged;
end;

procedure TdxTabFormattingInfoWithoutSort.OnEndUpdate;
begin
  // do nothing
end;

procedure TdxTabFormattingInfoWithoutSort.OnFirstBeginUpdate;
begin
// do nothing
end;

procedure TdxTabFormattingInfoWithoutSort.OnLastCancelUpdate;
begin
// do nothing
end;

procedure TdxTabFormattingInfoWithoutSort.OnLastEndUpdate;
begin
  OnChanged;
end;

{ TdxTabInfoComparer }

function TdxTabInfoComparer.Compare(const X, Y: TdxTabInfo): Integer;
begin
  Result := X.Position - Y.Position;
end;

{ TdxTabFormattingInfoCache }

function TdxTabFormattingInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTabFormattingInfo;
begin
  Result := TdxTabFormattingInfo.Create;
end;

{ TdxTabProperties }

constructor TdxTabProperties.Create(const AOwner: IdxParagraphPropertiesContainer);
begin
  inherited Create(AOwner.PieceTable);
  FOwner := AOwner;
end;

procedure TdxTabProperties.SetTabs(ATabs: TdxTabFormattingInfo);
begin
  if SuppressInsertTabs then
    Exit;
  if (Info.Count = 0) and (ATabs.Count = 0) then
    Exit;
  ReplaceInfo(ATabs, BatchUpdateChangeActions);
end;

function TdxTabProperties.GetTabs: TdxTabFormattingInfo;
begin
  Result := Info.Clone;
end;

function TdxTabProperties.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTabFormattingInfo>;
begin
  Result := TdxSimpleDocumentCache(ADocumentModel.Cache).TabFormattingInfoCache;
end;

procedure TdxTabProperties.SetTabCore(AIndex: Integer; const AValue: TdxTabInfo);
var
  ANewInfo: TdxTabFormattingInfo;
  AIsDeferred: Boolean;
begin
  ANewInfo := GetInfoForModification(AIsDeferred);
  ANewInfo[AIndex] := AValue;
  ReplaceInfo(ANewInfo, BatchUpdateChangeActions);
  if not AIsDeferred then ANewInfo.Free;
end;

function TdxTabProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.ForceResetHorizontalRuler];
end;

function TdxTabProperties.GetObtainAffectedRangeListener: IdxObtainAffectedRangeListener;
begin
  if not Supports(FOwner, IdxObtainAffectedRangeListener, Result) then
    Result := nil;
end;

function TdxTabProperties.SuppressInsertTabs: Boolean;
begin
  Result := not DocumentModel.DocumentCapabilities.ParagraphTabsAllowed;
end;

function TdxTabProperties.GetItem(Index: Integer): TdxTabInfo;
begin
  Result := Info[Index];
end;

procedure TdxTabProperties.SetItem(Index: Integer; const Value: TdxTabInfo);
begin
  if Info[Index] = Value then
    Exit;
  if SuppressInsertTabs then
    Exit;
  SetTabCore(Index, Value);
end;

end.
