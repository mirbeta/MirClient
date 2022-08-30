{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL           }
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

unit dxCustomTree;

{$I cxVer.inc}

interface

uses
  Classes;

type
  TdxTreeCustomNode = class;
  TdxTreeCustomNodeClass = class of TdxTreeCustomNode;

  TdxTreeNodeNotification = (tnStructure, tnData);
  TdxTreeNodeNotifications = set of TdxTreeNodeNotification;

  IdxTreeOwner = interface
  ['{E5BD359F-E1D0-4ABC-9D9D-45A6516F2F8B}']
    //
    function CanCollapse(ASender: TdxTreeCustomNode): Boolean;
    function CanExpand(ASender: TdxTreeCustomNode): Boolean;
    procedure Collapsed(ASender: TdxTreeCustomNode);
    procedure Expanded(ASender: TdxTreeCustomNode);
    procedure LoadChildren(ASender: TdxTreeCustomNode);
    //
    procedure BeforeDelete(ASender: TdxTreeCustomNode);
    procedure DeleteNode(ASender: TdxTreeCustomNode);
    //
    function GetOwner: TPersistent;
    function GetNodeClass(ARelativeNode: TdxTreeCustomNode): TdxTreeCustomNodeClass;
    //
    procedure TreeNotification(ASender: TdxTreeCustomNode; ANotification: TdxTreeNodeNotifications);
    //
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

  TdxTreeNodeAttachMode = (namAdd, namAddFirst, namAddChild, namAddChildFirst, namInsert);
  TdxTreeNodeAddMode = (amAdd, amAddFirst, amInsert);
  TdxTreeNodeState = (nsCollapsed, nsHasChildren, nsValidIndexes, nsDeleting, nsInternalDelete);
  TdxTreeNodeStates = set of TdxTreeNodeState;

  TdxCustomTreeNodeCompareProc = function(ANode1, ANode2: TdxTreeCustomNode): Integer;

  TdxTreeCustomNode = class
  private
    FCount: Integer;
    FData: Pointer;
    FFirst: TdxTreeCustomNode;
    FImageIndex: Integer;
    FIndex: Integer;
    FLast: TdxTreeCustomNode;
    FNext: TdxTreeCustomNode;
    FParent: TdxTreeCustomNode;
    FPrev: TdxTreeCustomNode;
    FState: TdxTreeNodeStates;
    FStructureID: Integer;

    function GetExpanded: Boolean;
    function GetHasChildren: Boolean;
    function GetIndex: Integer;
    function GetItem(AIndex: Integer): TdxTreeCustomNode;
    function GetLevel: Integer;
    function GetOwner: TPersistent;
    function GetRoot: TdxTreeCustomNode;
    procedure SetExpanded(AValue: Boolean);
    procedure SetHasChildren(AValue: Boolean);
    procedure SetImageIndex(AValue: Integer);
  protected
    FOwner: IdxTreeOwner;
    procedure AdjustIndexes;
    procedure ExtractFromParent;
    function GetImageIndex: Integer; virtual;
    procedure InternalInsert(AValue: TdxTreeCustomNode);
    procedure Notify(ANotification: TdxTreeNodeNotifications);
    procedure PopulateItems(AList: TList);
    procedure SetFirst(AValue: TdxTreeCustomNode);
    procedure SetLast(AValue: TdxTreeCustomNode);
    procedure SetNodeParent(ANewNode, ANewParent: TdxTreeCustomNode; AMode: TdxTreeNodeAddMode); virtual;
    procedure SetParentFor(AValue: TdxTreeCustomNode; AValidateIndexes: Boolean = True);
    procedure UpdateItems(AList: TList);

    procedure ReadData(AStream: TStream; const AVersion: Cardinal = 0); virtual;
    procedure WriteData(AStream: TStream); virtual;

    property State: TdxTreeNodeStates read FState write FState;
    property StructureID: Integer read FStructureID write FStructureID;
  public
    constructor Create(AOwner: IdxTreeOwner); virtual;
    destructor Destroy; override;
    function AddChild: TdxTreeCustomNode;
    function AddChildFirst: TdxTreeCustomNode;
    function AddNode(ANode, ARelative: TdxTreeCustomNode;
      AData: Pointer; AttachMode: TdxTreeNodeAttachMode): TdxTreeCustomNode;
    procedure CustomSort(ACompareProc: TdxCustomTreeNodeCompareProc; ARecurse: Boolean = False);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear; virtual;
    procedure DeleteChildren; virtual;
    function HasAsParent(ANode: TdxTreeCustomNode): Boolean;
    function IsRoot: Boolean;
    procedure LoadChildren; virtual;
    procedure MoveTo(ADestNode: TdxTreeCustomNode; AMode: TdxTreeNodeAttachMode); virtual;

    property Count: Integer read FCount;
    property Data: Pointer read FData write FData;
    property First: TdxTreeCustomNode read FFirst;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property HasChildren: Boolean read GetHasChildren write SetHasChildren;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property Index: Integer read GetIndex;
    property Items[Index: Integer]: TdxTreeCustomNode read GetItem; default;
    property Last: TdxTreeCustomNode read FLast;
    property Level: Integer read GetLevel;
    property Next: TdxTreeCustomNode read FNext;
    property Owner: TPersistent read GetOwner;
    property Parent: TdxTreeCustomNode read FParent;
    property Prev: TdxTreeCustomNode read FPrev;
    property Root: TdxTreeCustomNode read GetRoot;
  end;

  TdxTreeForEachNodeProc = function(ANode: TdxTreeCustomNode; AData: Pointer): Boolean of object;

  function dxTreeForEach(ARoot: TdxTreeCustomNode; AProc: TdxTreeForEachNodeProc; AData: Pointer): Integer;

implementation

uses
  RTLConsts;

function dxTreeForEach(ARoot: TdxTreeCustomNode;
  AProc: TdxTreeForEachNodeProc; AData: Pointer): Integer;
var
  ANode: TdxTreeCustomNode;
begin
  Result := 0;
  if ARoot = nil then Exit;
  ANode := ARoot;
  while ANode <> nil do
  begin
    if AProc(ANode, AData) then
      Inc(Result)
    else
      Break;
    if ANode.FFirst <> nil then
      ANode := ANode.FFirst
    else
      if ANode.FNext <> nil then
        ANode := ANode.FNext
      else
      begin
        while (ANode <> nil) and (ANode.FNext = nil) do
          ANode := ANode.Parent;
        if ANode <> nil then
          ANode := ANode.FNext;
      end;
  end;
end;

{ TdxTreeCustomNode }

constructor TdxTreeCustomNode.Create(AOwner: IdxTreeOwner);
begin
  FOwner := AOwner;
end;

destructor TdxTreeCustomNode.Destroy;
begin
  BeginUpdate;
  try
    Include(FState, nsDeleting);
    if not IsRoot then
      FOwner.BeforeDelete(Self);
    if Count > 0 then
      DeleteChildren;
    if not IsRoot then
      FOwner.DeleteNode(Self);
    ExtractFromParent;
  finally
    if not IsRoot then
      Notify([tnStructure, tnData]);
    EndUpdate;
  end;
  FOwner := nil;
  inherited Destroy;
end;

function TdxTreeCustomNode.AddChild: TdxTreeCustomNode;
begin
  Result := AddNode(nil, Self, nil, namAddChild);
end;

function TdxTreeCustomNode.AddChildFirst: TdxTreeCustomNode;
begin
  Result := AddNode(nil, Self, nil, namAddChildFirst);
end;

function TdxTreeCustomNode.AddNode(ANode, ARelative: TdxTreeCustomNode;
  AData: Pointer; AttachMode: TdxTreeNodeAttachMode): TdxTreeCustomNode;
const
  IsAddChild: array[TdxTreeNodeAttachMode] of Boolean =
    (False, False, True, True, False);
  AddMode: array[TdxTreeNodeAttachMode] of TdxTreeNodeAddMode =
    (amAdd, amAddFirst, amAdd, amAddFirst, amInsert);
begin
  if ANode = nil then
  begin
    Result := FOwner.GetNodeClass(ARelative).Create(FOwner);
    Result.FData := AData;
  end
  else
    Result := ANode;
  if (ARelative = nil) and (AttachMode = namInsert) then
    AttachMode := namAdd;
  if ARelative <> nil then
    SetNodeParent(Result, ARelative, AddMode[AttachMode])
  else
    SetNodeParent(Result, Root, AddMode[AttachMode]);
  if AddMode[AttachMode] = amInsert then
    Result.StructureID := Result.Index
  else
    if Result.StructureID < 0 then
      Result.StructureID  := Result.Parent.Count;
  Notify([tnStructure]);
end;

procedure TdxTreeCustomNode.CustomSort(
  ACompareProc: TdxCustomTreeNodeCompareProc; ARecurse: Boolean = False);
var
  I: Integer;
  AList: TList;
begin
  if not Assigned(ACompareProc) or (Count = 0) then Exit;
  BeginUpdate;
  try
    AList := TList.Create;
    try
      PopulateItems(AList);
      if AList.Count > 1 then
      begin
        AList.Sort(@ACompareProc);
        UpdateItems(AList);
      end;
      if ARecurse then
      begin
        for I := 0 to AList.Count - 1 do
          TdxTreeCustomNode(AList.List[I]).CustomSort(ACompareProc, ARecurse);
      end;
    finally
      AList.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxTreeCustomNode.BeginUpdate;
begin
  FOwner.BeginUpdate;
end;

procedure TdxTreeCustomNode.EndUpdate;
begin
  FOwner.EndUpdate;
end;

procedure TdxTreeCustomNode.Clear;
begin
  DeleteChildren;
end;

procedure TdxTreeCustomNode.DeleteChildren;
var
  ANode: TdxTreeCustomNode;
begin
  BeginUpdate;
  try
    while FFirst <> nil do
    begin
      if nsInternalDelete in State then
        Include(FFirst.FState, nsInternalDelete);
      ANode := FFirst;
      FFirst := FFirst.FNext;
      if not (nsInternalDelete in State) then
        FOwner.DeleteNode(ANode)
      else
        Include(ANode.FState, nsDeleting);
      ANode.Free;
    end;
  finally
    FCount := 0;
    HasChildren := False;
    FFirst := nil;
    FLast := nil;
    EndUpdate;
  end;
end;

function TdxTreeCustomNode.HasAsParent(ANode: TdxTreeCustomNode): Boolean;
var
  AItem: TdxTreeCustomNode;
begin
  Result := False;
  AItem := Parent;
  while (AItem <> nil) and not Result do
  begin
    Result := AItem = ANode;
    AItem := AItem.FParent;
  end;
end;

procedure TdxTreeCustomNode.LoadChildren;
begin
  if (Count <> 0) or not HasChildren or Expanded then Exit;
  BeginUpdate;
  try
    FOwner.LoadChildren(Self);
    Exclude(FState, nsHasChildren);
    Notify([tnStructure]);
  finally
    EndUpdate;
  end;
end;

procedure TdxTreeCustomNode.MoveTo(
  ADestNode: TdxTreeCustomNode; AMode: TdxTreeNodeAttachMode);
begin
  if (ADestNode = nil) or (ADestNode = Self) then Exit;
  if AMode in [namAdd, namAddFirst] then
    ADestNode := ADestNode.Parent;
  if (ADestNode = nil) or ADestNode.HasAsParent(Self) or
    ((ADestNode = Parent) and (AMode = namAddChild)) or (ADestNode = Self) then Exit;
  AddNode(Self, ADestNode, nil, AMode);
end;

function TdxTreeCustomNode.IsRoot: Boolean;
begin
  Result := Parent = nil;
end;

procedure TdxTreeCustomNode.AdjustIndexes;
var
  AIndex: Integer;
  ANode: TdxTreeCustomNode;
begin
  if nsValidIndexes in State then Exit;
  AIndex := 0;
  ANode := FFirst;
  while ANode <> nil do
  begin
    ANode.FIndex := AIndex;
    ANode := ANode.FNext;
    Inc(AIndex);
  end;
  Include(FState, nsValidIndexes);
end;

procedure TdxTreeCustomNode.ExtractFromParent;
begin
  if FParent <> nil then
  begin
    Exclude(FParent.FState, nsValidIndexes);
    Dec(FParent.FCount);
    if FParent.FFirst = Self then
      FParent.FFirst := FNext;
    if FParent.FLast = Self then
      FParent.FLast := FPrev;
    if FParent.FCount = 0 then
      FParent.State := FParent.FState - [nsHasChildren] + [nsCollapsed];
  end;
  if FNext <> nil then
    FNext.FPrev := FPrev;
  if FPrev <> nil then
    FPrev.FNext := FNext;
  FPrev := nil;
  FNext := nil;
  FParent := nil;
end;

function TdxTreeCustomNode.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;

procedure TdxTreeCustomNode.InternalInsert(AValue: TdxTreeCustomNode);
begin
  if FParent <> nil then
    ExtractFromParent;
  FPrev := AValue.FPrev;
  if FPrev <> nil then
    FPrev.FNext := Self
  else
    AValue.FParent.FFirst := Self;
  FNext := AValue;
  FNext.FPrev := Self;
  AValue.FParent.SetParentFor(Self);
end;

procedure TdxTreeCustomNode.Notify(ANotification: TdxTreeNodeNotifications);
begin
  FOwner.TreeNotification(Self, ANotification);
end;

procedure TdxTreeCustomNode.PopulateItems(AList: TList);
var
  I: Integer;
  ANode: TdxTreeCustomNode;
begin
  I := 0;
  AList.Count := Count;
  ANode := FFirst;
  while ANode <> nil do
  begin
    AList.List[I] := ANode;
    ANode := ANode.FNext;
    Inc(I);
  end;
end;

procedure TdxTreeCustomNode.SetFirst(AValue: TdxTreeCustomNode);
begin
  AValue.ExtractFromParent;
  if FFirst <> nil then
  begin
    AValue.FNext := FFirst;
    FFirst.FPrev := AValue;
  end
  else
    FLast := AValue;
  FFirst := AValue;
  SetParentFor(AValue, False);
end;

procedure TdxTreeCustomNode.SetLast(AValue: TdxTreeCustomNode);
begin
  AValue.ExtractFromParent;
  if FLast <> nil then
  begin
    AValue.FPrev := FLast;
    FLast.FNext := AValue;
  end
  else
    FFirst := AValue;
  FLast := AValue;
  SetParentFor(AValue, False);
end;

procedure TdxTreeCustomNode.SetNodeParent(ANewNode, ANewParent: TdxTreeCustomNode;
  AMode: TdxTreeNodeAddMode);
begin
  case AMode of
    amAdd:
      ANewParent.SetLast(ANewNode);
    amAddFirst:
      ANewParent.SetFirst(ANewNode);
    amInsert:
      ANewNode.InternalInsert(ANewParent);
  end;
  Notify([tnStructure]);
end;

procedure TdxTreeCustomNode.SetParentFor(
  AValue: TdxTreeCustomNode; AValidateIndexes: Boolean = True);
begin
  AValue.FParent := Self;
  if AValidateIndexes then
    Exclude(FState, nsValidIndexes)
  else
    AValue.FIndex := FCount;
  Inc(FCount);
  Notify([tnStructure]);
end;

procedure TdxTreeCustomNode.UpdateItems(AList: TList);
var
  I, L: Integer;
  ANode: TdxTreeCustomNode;
begin
  if Count = 0 then Exit;
  L := Count - 1;
  for I := 0 to L do
  begin
    ANode := TdxTreeCustomNode(AList.List[I]);
    ANode.FIndex := I;
    if I > 0 then
      ANode.FPrev := AList.List[I - 1]
    else
      ANode.FPrev := nil;
     if I < L then
       ANode.FNext := AList.List[I + 1]
     else
       ANode.FNext := nil;
  end;
  FFirst := TdxTreeCustomNode(AList.List[0]);
  FLast := TdxTreeCustomNode(AList.List[L]);
end;

procedure TdxTreeCustomNode.ReadData(AStream: TStream; const AVersion: Cardinal = 0);
var
  I, AChildrenCount: Integer;
begin
  DeleteChildren;
  AStream.ReadBuffer(FImageIndex, SizeOf(FImageIndex));
  AStream.ReadBuffer(AChildrenCount, SizeOf(AChildrenCount));
  for I := 0 to AChildrenCount - 1 do
    AddChild.ReadData(AStream, AVersion);
end;

procedure TdxTreeCustomNode.WriteData(AStream: TStream);
var
  I: Integer;
begin
  AStream.WriteBuffer(FImageIndex, SizeOf(FImageIndex));
  AStream.WriteBuffer(FCount, SizeOf(FCount));
  for I := 0 to Count - 1 do
    Items[I].WriteData(AStream);
end;

function TdxTreeCustomNode.GetExpanded: Boolean;
begin
  Result := (Count > 0) and (not (nsCollapsed in State) or (FParent = nil));
end;

function TdxTreeCustomNode.GetHasChildren: Boolean;
begin
  Result := (nsHasChildren in State) or (Count > 0);
end;

function TdxTreeCustomNode.GetIndex: Integer;
begin
  if Parent <> nil then
    Parent.AdjustIndexes;
  Result := FIndex;
end;

function TdxTreeCustomNode.GetLevel: Integer;
var
  ANode: TdxTreeCustomNode;
begin
  Result := -1;
  ANode := Parent;
  while ANode <> nil do
  begin
    Inc(Result);
    ANode := ANode.Parent;
  end;
end;

function TdxTreeCustomNode.GetItem(AIndex: Integer): TdxTreeCustomNode;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EListError.CreateFmt(SListIndexError, [AIndex]);
  if Parent <> nil then
    Parent.AdjustIndexes;
  if (FLast.Index shr 1) <= AIndex  then
  begin
    Result := FLast;
    while Result.FIndex <> AIndex do
      Result := Result.FPrev;
  end
  else
  begin
    Result := FFirst;
    while Result.FIndex <> AIndex do
      Result := Result.FNext;
  end;
end;

function TdxTreeCustomNode.GetOwner: TPersistent;
begin
  Result := FOwner.GetOwner;
end;

function TdxTreeCustomNode.GetRoot: TdxTreeCustomNode;
begin
  Result := Self;
  while Result.Parent <> nil do
    Result := Result.Parent;
end;

procedure TdxTreeCustomNode.SetExpanded(AValue: Boolean);
var
  AExpanded: Boolean;
begin
  if AValue <> Expanded then
  begin
    AExpanded := Expanded;
    if Parent = nil then Exit;
    BeginUpdate;
    try
      if AValue then
      begin
        if not FOwner.CanExpand(Self) then Exit;
        LoadChildren;
        Exclude(FState, nsCollapsed);
        FOwner.Expanded(Self);
      end
      else
      begin
        if not FOwner.CanCollapse(Self) then Exit;
        Include(FState, nsCollapsed);
        FOwner.Collapsed(Self);
      end;
      if AExpanded <> Expanded then
        Notify([tnStructure]);
    finally
      EndUpdate;
    end
  end;
end;

procedure TdxTreeCustomNode.SetHasChildren(AValue: Boolean);
begin
  if HasChildren <> AValue then
  begin
    if AValue then
      Include(FState, nsHasChildren)
    else
      if FCount = 0 then
        Exclude(FState, nsHasChildren);
    Notify([tnStructure]);
  end;
end;

procedure TdxTreeCustomNode.SetImageIndex(AValue: Integer);
begin
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    Notify([tnData]);
  end;
end;

end.
