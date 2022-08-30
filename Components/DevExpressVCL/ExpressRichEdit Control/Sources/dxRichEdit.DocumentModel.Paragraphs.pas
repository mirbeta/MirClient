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

unit dxRichEdit.DocumentModel.Paragraphs;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, Contnrs,
  dxCore, dxCoreClasses,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem;

type
  TdxCustomParagraph = class;
  TdxIndexedTreeNodeBase = class;
  TdxIndexedTreeNodeMiddleLevel = class;

  { TdxIndexedTreeNodeBaseList }

  TdxIndexedTreeNodeBaseList = class(TdxFastObjectList)
  strict private
    function GetItem(Index: Integer): TdxIndexedTreeNodeBase;
  public
    property Items[Index: Integer]: TdxIndexedTreeNodeBase read GetItem; default;
  end;

  { TdxIndexedTreeNodeBase }

  TdxIndexedTreeNodeBase = class abstract
  strict private
    FMaxChildrenCount: Integer;
    FFirstRelativeIndex: Integer;
    FParent: TdxIndexedTreeNodeMiddleLevel;
    FRelativeLastRunIndex: Integer;
    FRelativeLogPosition: Integer;
    FRunIndexCount: Integer;
    FRelativeFirstRunIndex: Integer;
  public
    constructor Create(AMaxChildrenCount: Integer);

    function Insert(ARelativeIndex: Integer; AItem: TdxCustomParagraph): TdxIndexedTreeNodeBase; virtual; abstract;
    function RemoveAt(ARelativeIndex: Integer): Boolean; virtual; abstract;
    function FindItemByIndex(ARelativeIndex: Integer): TdxCustomParagraph; virtual; abstract;
    function First: TdxCustomParagraph; virtual; abstract;
    function Last: TdxCustomParagraph; virtual; abstract;
    procedure RecalcParagraphsPositionsCore(ARelativeFrom, ARelativeTo, ATotalDescendantCount, ADeltaLength, ADeltaRunIndex: Integer); virtual; abstract;
    procedure OnEndMultipleRunSplit; virtual; abstract;

    property MaxChildrenCount: Integer read FMaxChildrenCount;
    property FirstRelativeIndex: Integer read FFirstRelativeIndex write FFirstRelativeIndex;
    property RelativeFirstRunIndex: Integer read FRelativeFirstRunIndex write FRelativeFirstRunIndex;
    property RelativeLastRunIndex: Integer read FRelativeLastRunIndex write FRelativeLastRunIndex;
    property RelativeLogPosition: Integer read FRelativeLogPosition write FRelativeLogPosition;
    property RunIndexCount: Integer read FRunIndexCount write FRunIndexCount;
    property Parent: TdxIndexedTreeNodeMiddleLevel read FParent write FParent;
  end;

  { TdxIndexedTreeNodeMiddleLevel }

  TdxIndexedTreeNodeMiddleLevel = class(TdxIndexedTreeNodeBase)
  strict private
    FChildren: TdxIndexedTreeNodeBaseList;
    function GetChildrenCount: Integer;
  public
    constructor Create(AMaxChildrenCount: Integer);
    destructor Destroy; override;
    function First: TdxCustomParagraph; override;
    function Last: TdxCustomParagraph; override;
    function Insert(ARelativeIndex: Integer; AItem: TdxCustomParagraph): TdxIndexedTreeNodeBase; override;
    function RemoveAt(ARelativeIndex: Integer): Boolean; override;
    function CreateNew: TdxIndexedTreeNodeMiddleLevel;
    function SplitNode(ADeltaIndex: Integer): TdxIndexedTreeNodeMiddleLevel;
    function FindItemByIndex(ARelativeIndex: Integer): TdxCustomParagraph; override;
    function ToString: string; override;
    procedure AddChild(AChild: TdxIndexedTreeNodeBase);
    procedure RecalcParagraphsPositionsCore(ARelativeFrom, ARelativeTo, ATotalDescendantCount, ADeltaLength, ADeltaRunIndex: Integer); override;
    procedure ShiftRelativeFirstRunIndex(ADeltaRunIndex: Integer);
    procedure ShiftRelativeLogPosition(ADeltaLogPosition: Integer);
    procedure OnEndMultipleRunSplit; override;

    property ChildrenCount: Integer read GetChildrenCount;
    property Children: TdxIndexedTreeNodeBaseList read FChildren;
  end;

  { TdxCustomParagraph }

  TdxCustomParagraph = class abstract(TdxParagraphBase, IdxParagraphPropertiesContainer)
  strict private
    FRelativeIndex: Integer;
    FRelativeFirstRunIndex: Integer;
    FRelativeLastRunIndex: Integer;
    FRelativeLogPosition: Integer;
    FParagraphProperties: TdxParagraphProperties;
    FParent: TdxIndexedTreeNodeBase;
    function GetDocumentModel: TdxCustomDocumentModel;
    procedure SetRelativeLastRunIndex(ARunIndex: TdxRunIndex);
  strict protected
    function IdxParagraphPropertiesContainer.GetPieceTable = GetPropertiesContainerPieceTable;
    function GetPropertiesContainerPieceTable: TdxCustomPieceTable;
    function CreateParagraphPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore; virtual; abstract;
    procedure OnParagraphPropertiesChanged; virtual; abstract;

    function GetFirstRunIndex: TdxRunIndex; override;
    function GetEndLogPosition: TdxDocumentLogPosition; override;
    function GetLastRunIndex: TdxRunIndex; override;
    function GetLogPosition: TdxDocumentLogPosition; override;
    function GetIndex: TdxParagraphIndex; override;

    procedure SetRelativeFirstRunIndex(ARunIndex: TdxRunIndex);
    procedure SetRelativeLogPosition(ALogPosition: TdxDocumentLogPosition);
  public
    constructor Create(APieceTable: TdxCustomPieceTable); override;
    destructor Destroy; override;

    procedure AfterRemove(AFirstRunIndex, ALastRunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition);
    procedure AfterUndoRemove;

    procedure ShiftRelativeIndex(ADelta: Integer);
    procedure ShiftRelativePosition(ADeltaLength, ADeltaFirstRunIndex, ADeltaLastRunIndex: Integer);

    procedure SetRelativeLastRunIndexWithoutCheck(ARunIndex: TdxRunIndex);
    procedure ShiftLogPosition(ADeltaLength: Integer);

    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
    property ParagraphProperties: TdxParagraphProperties read FParagraphProperties;
    property Parent: TdxIndexedTreeNodeBase read FParent write FParent;
    property RelativeIndex: Integer read FRelativeIndex write FRelativeIndex;
    property RelativeFirstRunIndex: TdxRunIndex read FRelativeFirstRunIndex write SetRelativeFirstRunIndex;
    property RelativeLastRunIndex: Integer read FRelativeLastRunIndex write SetRelativeLastRunIndex;
    property RelativeLogPosition: TdxDocumentLogPosition read FRelativeLogPosition write SetRelativeLogPosition;
  end;

  { TdxCustomParagraphList }

  TdxCustomParagraphList = class(TdxFastObjectList)
  strict private
    function GetItem(Index: Integer): TdxCustomParagraph;
    procedure SetItem(Index: Integer; const Value: TdxCustomParagraph);
  public
    function First: TdxCustomParagraph; reintroduce;
    function Last: TdxCustomParagraph; reintroduce;
    property Items[Index: Integer]: TdxCustomParagraph read GetItem write SetItem; default;
  end;

  { TdxIndexedTreeNodeLeafLevel }

  TdxIndexedTreeNodeLeafLevel = class(TdxIndexedTreeNodeBase)
  strict private
    FItems: TdxCustomParagraphList;
    function GetChildrenCount: Integer;
  public
    constructor Create(AMaxChildrenCount: Integer);
    destructor Destroy; override;

    function First: TdxCustomParagraph; override;
    function Last: TdxCustomParagraph; override;
    function Insert(ARelativeIndex: Integer; AItem: TdxCustomParagraph): TdxIndexedTreeNodeBase; override;
    procedure InsertWithShiftIndex(ATargetIndex: Integer; AItem: TdxCustomParagraph);
    function SplitNode: TdxIndexedTreeNodeLeafLevel;
    function CreateNew: TdxIndexedTreeNodeLeafLevel;
    function RemoveAt(ARelativeIndex: Integer): Boolean; override;
    function FindItemByIndex(ARelativeIndex: Integer): TdxCustomParagraph; override;
    function ToString: string; override;
    procedure RecalcParagraphsPositionsCore(AFrom, ATo, ATotalDescendantCount, ADeltaLength, ADeltaRunIndex: Integer); override;
    procedure ShiftRelativeFirstRunIndex(ADeltaRunIndex: Integer);
    procedure ShiftRelativeLogPosition(ADeltaLogPosition: Integer);
    procedure OnEndMultipleRunSplit; override;
    procedure EnsureValidRelativeFirstRunIndex(AItem: TdxCustomParagraph; ARelativeFirstRunIndex: Integer);
    procedure EnsureValidRelativeLogPosition(AItem: TdxCustomParagraph; ARelativeLogPosition: Integer);

    property Items: TdxCustomParagraphList read FItems;
    property ChildrenCount: Integer read GetChildrenCount;
  end;

  { TdxIndexedTree }

  TdxIndexedTree = class abstract(TdxParagraphBaseCollection)
  public const
    DefaultMaxChildrenCount = 16;
  strict private
    FMaxChildrenCount: Integer;
    FRoot: TdxIndexedTreeNodeBase;
    FCount: Integer;
    function GetItem(Index: Integer): TdxCustomParagraph;
  strict protected
    function GetCount: Integer; override;
    function GetItemBase(Index: Integer): TdxParagraphBase; override;
  public
    constructor Create(AMaxChildrenCount: Integer = DefaultMaxChildrenCount);
    destructor Destroy; override;
    function CreateLeafLevel: TdxIndexedTreeNodeLeafLevel;
    function CreateMiddleLevel: TdxIndexedTreeNodeMiddleLevel;
    function FindItemByIndex(AIndex: TdxParagraphIndex): TdxCustomParagraph;
    procedure Add(AItem: TdxCustomParagraph);
    procedure Insert(AIndex: TdxParagraphIndex; AItem: TdxCustomParagraph);
    procedure InsertCore(AIndex: Integer; AItem: TdxCustomParagraph);
    procedure RemoveAt(ARelativeIndex: TdxParagraphIndex);
    procedure Clear; override;
    procedure ForEach(const AAction: TdxAction<TdxCustomParagraph>);
    procedure AddRange(const ACollection: TdxList<TdxCustomParagraph>);
    function GetRange(AIndex, ACount: Integer): TdxList<TdxCustomParagraph>;

    function IndexOf(AItem: TdxCustomParagraph): TdxParagraphIndex;
    function First: TdxCustomParagraph;
    function Last: TdxCustomParagraph;
    function GetFirstCore: TdxParagraphBase; override;
    function GetLastCore: TdxParagraphBase; override;

    property Items[Index: Integer]: TdxCustomParagraph read GetItem; default;
    property MaxChildrenCount: Integer read FMaxChildrenCount;
    property Root: TdxIndexedTreeNodeBase read FRoot;
  end;

  { TdxParagraphIndexedTree }

  TdxParagraphIndexedTree = class abstract(TdxIndexedTree)
  public
    function SearchByLogPosition(ALogPosition: TdxDocumentLogPosition): TdxParagraphIndex; override;
    procedure RecalcParagraphsPositionsCore(AFrom, ATo: TdxParagraphIndex; ADeltaLength, ADeltaRunIndex: Integer);
  end;

  { TdxCustomParagraphCollection }

  TdxCustomParagraphCollection = class(TdxParagraphIndexedTree)
  public
    procedure OnBeginMultipleRunSplit;
    procedure OnEndMultipleRunSplit;
  end;

implementation

uses
  Math, RTLConsts,

  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs;

{ TdxIndexedTreeNodeBaseList }

function TdxIndexedTreeNodeBaseList.GetItem(Index: Integer): TdxIndexedTreeNodeBase;
begin
  Result := TdxIndexedTreeNodeBase(List[Index]);
end;

{ TdxIndexedTreeNodeBase }

constructor TdxIndexedTreeNodeBase.Create(AMaxChildrenCount: Integer);
begin
  inherited Create;
  FMaxChildrenCount := AMaxChildrenCount;
end;

{ TdxIndexedTreeNodeMiddleLevel }

procedure TdxIndexedTreeNodeMiddleLevel.AddChild(AChild: TdxIndexedTreeNodeBase);
begin
  Children.Add(AChild);
  AChild.Parent := Self;
end;

constructor TdxIndexedTreeNodeMiddleLevel.Create(AMaxChildrenCount: Integer);
begin
  inherited Create(AMaxChildrenCount);
  FChildren := TdxIndexedTreeNodeBaseList.Create;
end;

destructor TdxIndexedTreeNodeMiddleLevel.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

function TdxIndexedTreeNodeMiddleLevel.CreateNew: TdxIndexedTreeNodeMiddleLevel;
begin
  Result := TdxIndexedTreeNodeMiddleLevel.Create(MaxChildrenCount);
end;

function TdxIndexedTreeNodeMiddleLevel.FindItemByIndex(ARelativeIndex: Integer): TdxCustomParagraph;
var
  I: Integer;
  APrevChildren: TdxIndexedTreeNodeBase;
begin
  I := 1;
  while I < Children.Count do
  begin
    if Children[I].FirstRelativeIndex > ARelativeIndex then
      Break;
    Inc(I);
  end;
  APrevChildren := Children[I - 1];
  Result := APrevChildren.FindItemByIndex(ARelativeIndex - APrevChildren.FirstRelativeIndex);
end;

function TdxIndexedTreeNodeMiddleLevel.First: TdxCustomParagraph;
begin
  Assert(ChildrenCount > 0);
  Result := Children[0].First;
end;

function TdxIndexedTreeNodeMiddleLevel.GetChildrenCount: Integer;
begin
  Result := FChildren.Count;
end;

function TdxIndexedTreeNodeMiddleLevel.Insert(ARelativeIndex: Integer; AItem: TdxCustomParagraph): TdxIndexedTreeNodeBase;
var
  AChildIndex, ANewChildIndex, ADeltaIndex: Integer;
  ANewChildNode: TdxIndexedTreeNodeBase;
  ARightNode: TdxIndexedTreeNodeMiddleLevel;
begin
  Result := nil;
  Assert(ChildrenCount > 0);
  AChildIndex := ChildrenCount - 1;
  while ARelativeIndex < Children[AChildIndex].FirstRelativeIndex  do
  begin
    Children[AChildIndex].FirstRelativeIndex := Children[AChildIndex].FirstRelativeIndex + 1;
    Dec(AChildIndex);
  end;
  ANewChildNode := Children[AChildIndex].Insert(ARelativeIndex - Children[AChildIndex].FirstRelativeIndex, AItem);
  if ANewChildNode = nil then
    Exit;
  if ChildrenCount < MaxChildrenCount then
  begin
    Children.Insert(AChildIndex + 1, ANewChildNode);
    ANewChildNode.Parent := Self;
    Exit;
  end;
  ANewChildIndex := AChildIndex + 1;
  ADeltaIndex := Children[MaxChildrenCount div 2].FirstRelativeIndex;
  ARightNode := SplitNode(ADeltaIndex);
  if (ANewChildIndex > MaxChildrenCount / 2) then
  begin
    ANewChildNode.FirstRelativeIndex := ANewChildNode.FirstRelativeIndex - ADeltaIndex;
    ARightNode.Children.Insert(ANewChildIndex - MaxChildrenCount div 2, ANewChildNode);
    ANewChildNode.Parent := ARightNode;
    ANewChildNode.RelativeFirstRunIndex := ANewChildNode.RelativeFirstRunIndex - (ARightNode.RelativeFirstRunIndex - RelativeFirstRunIndex);
    ANewChildNode.RelativeLastRunIndex := ANewChildNode.RelativeLastRunIndex - (ARightNode.RelativeLastRunIndex - RelativeLastRunIndex);
    ANewChildNode.RelativeLogPosition := ANewChildNode.RelativeLogPosition - (ARightNode.RelativeLogPosition - RelativeLogPosition);
  end
  else
  begin
    Children.Insert(ANewChildIndex, ANewChildNode);
    ANewChildNode.Parent := Self;
  end;
  Result := ARightNode;
end;

function TdxIndexedTreeNodeMiddleLevel.Last: TdxCustomParagraph;
begin
  Assert(ChildrenCount > 0);
  Result := Children[ChildrenCount - 1].Last;
end;

procedure TdxIndexedTreeNodeMiddleLevel.OnEndMultipleRunSplit;
var
  I, ARelativeFirstRunIndex, ARelativeLogPosition: Integer;
begin
  for I := 0 to Children.Count - 1 do
    Children[I].OnEndMultipleRunSplit;
  ARelativeFirstRunIndex := Children[0].RelativeFirstRunIndex;
  if ARelativeFirstRunIndex <>  0 then
    ShiftRelativeFirstRunIndex(ARelativeFirstRunIndex);
  ARelativeLogPosition := Children[0].RelativeLogPosition;
  if ARelativeLogPosition <> 0 then
    ShiftRelativeFirstRunIndex(ARelativeLogPosition);
end;

procedure TdxIndexedTreeNodeMiddleLevel.RecalcParagraphsPositionsCore(ARelativeFrom, ARelativeTo, ATotalDescendantCount,
  ADeltaLength, ADeltaRunIndex: Integer);
var
  AChild: TdxIndexedTreeNodeBase;
  I, ANextChildFirstRelativeIndex: Integer;
begin
  Assert(ChildrenCount > 0);
  if (ARelativeFrom <= 0) and (ARelativeTo >= ATotalDescendantCount - 1) then
  begin
      RelativeFirstRunIndex := RelativeFirstRunIndex + ADeltaRunIndex;
      RelativeLastRunIndex := RelativeLastRunIndex + ADeltaRunIndex;
      RelativeLogPosition := RelativeLogPosition + ADeltaLength;
      Exit;
  end;
  ARelativeFrom := Max(0, ARelativeFrom);
  ARelativeTo := Min(ARelativeTo, ATotalDescendantCount - 1);
  ANextChildFirstRelativeIndex := ATotalDescendantCount;
  for I := ChildrenCount - 1 downto 0 do
  begin
    AChild := Children[I];
    if AChild.FirstRelativeIndex > ARelativeTo then
      Continue;
    if ANextChildFirstRelativeIndex <= ARelativeFrom then
      Break;
    AChild.RecalcParagraphsPositionsCore(ARelativeFrom - AChild.FirstRelativeIndex,
      ARelativeTo - AChild.FirstRelativeIndex, ANextChildFirstRelativeIndex - Children[I].FirstRelativeIndex,
      ADeltaLength, ADeltaRunIndex);
    ANextChildFirstRelativeIndex := Children[I].FirstRelativeIndex;
  end;
end;

function TdxIndexedTreeNodeMiddleLevel.RemoveAt(ARelativeIndex: Integer): Boolean;
var
  AChildIndex, AFirstRunIndex, AFirstLogPosition: Integer;
  AShouldRemoveChild: Boolean;
  AChild: TdxIndexedTreeNodeBase;
begin
  Assert(ChildrenCount > 0);
  AChildIndex := ChildrenCount - 1;
  while ARelativeIndex < Children[AChildIndex].FirstRelativeIndex do
  begin
    Children[AChildIndex].FirstRelativeIndex := Children[AChildIndex].FirstRelativeIndex - 1;
    Dec(AChildIndex);
  end;
  AShouldRemoveChild := Children[AChildIndex].RemoveAt(ARelativeIndex - Children[AChildIndex].FirstRelativeIndex);
  if AShouldRemoveChild then
  begin

    if Children[AChildIndex] is TdxIndexedTreeNodeLeafLevel then
      TdxIndexedTreeNodeLeafLevel(Children[AChildIndex]).Items.OwnsObjects := False;

    Children.Remove(Children[AChildIndex]);
    if (AChildIndex = 0) and (Children.Count > 0) then
    begin
      AChild := Children[AChildIndex];
      AFirstRunIndex := AChild.RelativeFirstRunIndex;
      if AFirstRunIndex > 0 then
        ShiftRelativeFirstRunIndex(AFirstRunIndex);
      AFirstLogPosition := AChild.RelativeLogPosition;
      if AFirstLogPosition > 0 then
        ShiftRelativeLogPosition(AFirstLogPosition);
    end;
  end;
  Result := Children.Count = 0;
end;

procedure TdxIndexedTreeNodeMiddleLevel.ShiftRelativeFirstRunIndex(ADeltaRunIndex: Integer);
var
  I: Integer;
begin
  RelativeFirstRunIndex := RelativeFirstRunIndex + ADeltaRunIndex;
  for I := 0 to FChildren.Count - 1 do
    Children[I].RelativeFirstRunIndex := Children[I].RelativeFirstRunIndex - ADeltaRunIndex;
  if (Parent <> nil) and ((RelativeFirstRunIndex < 0) or ((Parent.Children[0] = Self) and (RelativeFirstRunIndex > 0))) then
    Parent.ShiftRelativeFirstRunIndex(RelativeFirstRunIndex);
end;

procedure TdxIndexedTreeNodeMiddleLevel.ShiftRelativeLogPosition(ADeltaLogPosition: Integer);
var
  I: Integer;
begin
  RelativeLogPosition := RelativeLogPosition + ADeltaLogPosition;
  for I := 0 to FChildren.Count - 1 do
    FChildren[i].RelativeLogPosition := FChildren[i].RelativeLogPosition - ADeltaLogPosition;
  if (Parent <> nil) and ((RelativeLogPosition < 0) or ((Parent.Children[0] = Self) and (RelativeLogPosition > 0))) then
    Parent.ShiftRelativeLogPosition(RelativeLogPosition);
end;

function TdxIndexedTreeNodeMiddleLevel.SplitNode(ADeltaIndex: Integer): TdxIndexedTreeNodeMiddleLevel;
var
  AChild: TdxIndexedTreeNodeBase;
  I, ADeltaFirstRunIndex, ADeltaLastRunIndex, ADeltaLogPosition, AHalfCount: Integer;
begin
  Result := CreateNew;
  AHalfCount := MaxChildrenCount div 2;
  Result.FirstRelativeIndex := ADeltaIndex + FirstRelativeIndex;
  ADeltaFirstRunIndex := Children[AHalfCount].RelativeFirstRunIndex;
  ADeltaLastRunIndex := Children[AHalfCount].RelativeLastRunIndex;
  ADeltaLogPosition := Children[AHalfCount].RelativeLogPosition;
  Result.RelativeFirstRunIndex := RelativeFirstRunIndex + ADeltaFirstRunIndex;
  Result.RelativeLastRunIndex := RelativeLastRunIndex + ADeltaLastRunIndex;
  Result.RelativeLogPosition := RelativeLogPosition + ADeltaLogPosition;
  for I := AHalfCount to MaxChildrenCount - 1 do
  begin
    AChild := Children[I];
    Result.AddChild(AChild);
    AChild.FirstRelativeIndex := AChild.FirstRelativeIndex - ADeltaIndex;
    AChild.Parent := Result;
    AChild.RelativeFirstRunIndex := AChild.RelativeFirstRunIndex - ADeltaFirstRunIndex;
    AChild.RelativeLastRunIndex := AChild.RelativeLastRunIndex - ADeltaLastRunIndex;
    AChild.RelativeLogPosition := AChild.RelativeLogPosition - ADeltaLogPosition;
  end;
  Children.ExtractRange(AHalfCount, MaxChildrenCount - AHalfCount);
end;

function TdxIndexedTreeNodeMiddleLevel.ToString: string;
var
  S: string;
  I: Integer;
begin
  S := '';
  for I := 0 to ChildrenCount - 1 do
    if I = 0 then
      S := Children[i].ToString
    else
      S := S + ',' + Children[i].ToString;
  Result := Format('+%d,%d,%d:(%s)', [FirstRelativeIndex, RelativeFirstRunIndex, RelativeLogPosition, S]);
end;

{ TdxCustomParagraph }

constructor TdxCustomParagraph.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create(APieceTable);
  FParagraphProperties := TdxParagraphProperties.Create(Self);
end;

destructor TdxCustomParagraph.Destroy;
begin
  FreeAndNil(FParagraphProperties);
  inherited Destroy;
end;

procedure TdxCustomParagraph.AfterRemove(AFirstRunIndex, ALastRunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition);
begin
  FRelativeFirstRunIndex := AFirstRunIndex;
  FRelativeLastRunIndex := ALastRunIndex;
  FRelativeLogPosition := ALogPosition;
end;

procedure TdxCustomParagraph.AfterUndoRemove;
begin
  SetRelativeFirstRunIndex(FRelativeFirstRunIndex);
  SetRelativeLastRunIndexWithoutCheck(FRelativeLastRunIndex);
  SetRelativeLogPosition(FRelativeLogPosition);
end;

function TdxCustomParagraph.GetPropertiesContainerPieceTable: TdxCustomPieceTable;
begin
  Result := PieceTable;
end;

procedure TdxCustomParagraph.SetRelativeFirstRunIndex(ARunIndex: TdxRunIndex);
var
  AParentLevel: TdxIndexedTreeNodeMiddleLevel;
begin
  if Parent = nil then
    TdxRichEditExceptions.ThrowInternalException;
  if ARunIndex < 0 then
    TdxRichEditExceptions.ThrowInternalException;
  FRelativeFirstRunIndex := ARunIndex - Parent.RelativeFirstRunIndex;
  AParentLevel := Parent.Parent as TdxIndexedTreeNodeMiddleLevel;
  while AParentLevel <> nil do
  begin
    FRelativeFirstRunIndex := RelativeFirstRunIndex - AParentLevel.RelativeFirstRunIndex;
    AParentLevel := AParentLevel.Parent as TdxIndexedTreeNodeMiddleLevel;
  end;
  (Parent as TdxIndexedTreeNodeLeafLevel).EnsureValidRelativeFirstRunIndex(Self, FRelativeFirstRunIndex);
end;

function TdxCustomParagraph.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxCustomParagraph.GetLastRunIndex: TdxRunIndex;
var
  AParentLevel: TdxIndexedTreeNodeMiddleLevel;
begin
  if Parent = nil then
    TdxRichEditExceptions.ThrowInternalException;
  Result := Parent.RelativeLastRunIndex + FRelativeLastRunIndex;
  AParentLevel := Parent.Parent as TdxIndexedTreeNodeMiddleLevel;
  while AParentLevel <> nil do
  begin
    Result := Result + AParentLevel.RelativeLastRunIndex;
    AParentLevel := AParentLevel.Parent as TdxIndexedTreeNodeMiddleLevel;
  end;
end;

function TdxCustomParagraph.GetFirstRunIndex: TdxRunIndex;
var
 AParentLevel: TdxIndexedTreeNodeMiddleLevel;
begin
  if Parent = nil then
    TdxRichEditExceptions.ThrowInternalException;
  Result := Parent.RelativeFirstRunIndex + FRelativeFirstRunIndex;
  AParentLevel := Parent.Parent;
  while AParentLevel <> nil do
  begin
    Result := Result + AParentLevel.RelativeFirstRunIndex;
    AParentLevel := AParentLevel.Parent;
  end;
end;

function TdxCustomParagraph.GetEndLogPosition: TdxDocumentLogPosition;
begin
  Result := LogPosition + Length - 1;
end;

function TdxCustomParagraph.GetIndex: TdxParagraphIndex;
var
  AParentLevel: TdxIndexedTreeNodeMiddleLevel;
begin
  Assert(Parent <> nil);
  Result := Parent.FirstRelativeIndex + FRelativeIndex;
  AParentLevel := Parent.Parent as TdxIndexedTreeNodeMiddleLevel;
  while AParentLevel <> nil do
  begin
    Result := Result + AParentLevel.FirstRelativeIndex;
    AParentLevel := AParentLevel.Parent as TdxIndexedTreeNodeMiddleLevel;
  end;
end;

function TdxCustomParagraph.GetLogPosition: TdxDocumentLogPosition;
var
  AParentLevel: TdxIndexedTreeNodeMiddleLevel;
begin
  if Parent = nil then
    Result := -1
  else
  begin
    Result := Parent.RelativeLogPosition + FRelativeLogPosition;
    AParentLevel := Parent.Parent as TdxIndexedTreeNodeMiddleLevel;
    while AParentLevel <> nil do
    begin
      Result := Result + AParentLevel.RelativeLogPosition;
      AParentLevel := AParentLevel.Parent as TdxIndexedTreeNodeMiddleLevel;
    end;
  end;
end;

procedure TdxCustomParagraph.SetRelativeLastRunIndex(ARunIndex: TdxRunIndex);
var
  AParentLevel: TdxIndexedTreeNodeMiddleLevel;
begin
  if FParent = nil then
    TdxRichEditExceptions.ThrowInternalException;

  if ARunIndex < 0 then
    TdxRichEditExceptions.ThrowArgumentException('runIndex', ARunIndex);

  FRelativeLastRunIndex := ARunIndex - FParent.RelativeLastRunIndex;
  AParentLevel := TdxIndexedTreeNodeMiddleLevel(FParent.Parent);
  while AParentLevel <> nil do
  begin
    Dec(FRelativeLastRunIndex, AParentLevel.RelativeLastRunIndex);
    AParentLevel := TdxIndexedTreeNodeMiddleLevel(AParentLevel.Parent);
  end;
end;

procedure TdxCustomParagraph.SetRelativeLastRunIndexWithoutCheck(ARunIndex: TdxRunIndex);
var
  AParentLevel: TdxIndexedTreeNodeMiddleLevel;
begin
  if FParent = nil then
    TdxRichEditExceptions.ThrowInternalException;

  FRelativeLastRunIndex := ARunIndex - FParent.RelativeLastRunIndex;
  AParentLevel := TdxIndexedTreeNodeMiddleLevel(FParent.Parent);
  while AParentLevel <> nil do
  begin
    Dec(FRelativeLastRunIndex, AParentLevel.RelativeLastRunIndex);
    AParentLevel := TdxIndexedTreeNodeMiddleLevel(AParentLevel.Parent);
  end;
end;

procedure TdxCustomParagraph.ShiftLogPosition(ADeltaLength: Integer);
begin
  ShiftRelativePosition(ADeltaLength, 0, 0);
  TdxIndexedTreeNodeLeafLevel(Parent).EnsureValidRelativeLogPosition(Self, FRelativeLogPosition);
end;

procedure TdxCustomParagraph.SetRelativeLogPosition(ALogPosition: TdxDocumentLogPosition);
var
  AParentLevel: TdxIndexedTreeNodeMiddleLevel;
begin
  if Parent = nil then
    TdxRichEditExceptions.ThrowInternalException;
  if ALogPosition < 0 then
    TdxRichEditExceptions.ThrowArgumentException('logPosition', ALogPosition);
  FRelativeLogPosition := ALogPosition - Parent.RelativeLogPosition;
  AParentLevel := Parent.Parent as TdxIndexedTreeNodeMiddleLevel;
  while AParentLevel <> nil do
  begin
    FRelativeLogPosition := RelativeLogPosition - AParentLevel.RelativeLogPosition;
    AParentLevel := AParentLevel.Parent as TdxIndexedTreeNodeMiddleLevel;
  end;
  (Parent as TdxIndexedTreeNodeLeafLevel).EnsureValidRelativeLogPosition(Self, FRelativeLogPosition);
end;

procedure TdxCustomParagraph.ShiftRelativeIndex(ADelta: Integer);
begin
  Inc(FRelativeIndex, ADelta);
end;

procedure TdxCustomParagraph.ShiftRelativePosition(ADeltaLength, ADeltaFirstRunIndex, ADeltaLastRunIndex: Integer);
begin
  Inc(FRelativeFirstRunIndex, ADeltaFirstRunIndex);
  Inc(FRelativeLastRunIndex, ADeltaLastRunIndex);
  Inc(FRelativeLogPosition, ADeltaLength);
  if FRelativeFirstRunIndex < 0 then
    TdxIndexedTreeNodeLeafLevel(Parent).ShiftRelativeFirstRunIndex(FRelativeFirstRunIndex);
end;

{ TdxCustomParagraphList }

function TdxCustomParagraphList.First: TdxCustomParagraph;
begin
  Result := TdxCustomParagraph(inherited First);
end;

function TdxCustomParagraphList.Last: TdxCustomParagraph;
begin
  Result := TdxCustomParagraph(inherited Last);
end;

function TdxCustomParagraphList.GetItem(Index: Integer): TdxCustomParagraph;
begin
  Result := TdxCustomParagraph(List[Index]);
end;

procedure TdxCustomParagraphList.SetItem(Index: Integer; const Value: TdxCustomParagraph);
begin
  inherited Items[Index] := Value;
end;

{ TdxIndexedTreeNodeLeafLevel }

constructor TdxIndexedTreeNodeLeafLevel.Create(AMaxChildrenCount: Integer);
begin
  inherited Create(AMaxChildrenCount);
  FItems := TdxCustomParagraphList.Create;
end;

destructor TdxIndexedTreeNodeLeafLevel.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxIndexedTreeNodeLeafLevel.CreateNew: TdxIndexedTreeNodeLeafLevel;
begin
  Result := TdxIndexedTreeNodeLeafLevel.Create(MaxChildrenCount);
end;

procedure TdxIndexedTreeNodeLeafLevel.EnsureValidRelativeFirstRunIndex(AItem: TdxCustomParagraph; ARelativeFirstRunIndex: Integer);
begin
  if (ARelativeFirstRunIndex < 0) or ((AItem = FItems[0]) and (ARelativeFirstRunIndex > 0)) then
    ShiftRelativeFirstRunIndex(ARelativeFirstRunIndex);
end;

procedure TdxIndexedTreeNodeLeafLevel.EnsureValidRelativeLogPosition(AItem: TdxCustomParagraph; ARelativeLogPosition: Integer);
begin
  if (ARelativeLogPosition < 0) or (AItem = FItems[0]) and (ARelativeLogPosition > 0) then
    ShiftRelativeLogPosition(ARelativeLogPosition);
end;

function TdxIndexedTreeNodeLeafLevel.FindItemByIndex(ARelativeIndex: Integer): TdxCustomParagraph;
begin
  Result := Items[ARelativeIndex];
end;

function TdxIndexedTreeNodeLeafLevel.First: TdxCustomParagraph;
begin
  if ChildrenCount <= 0 then
    Result := nil
  else
    Result := Items[0];
end;

function TdxIndexedTreeNodeLeafLevel.GetChildrenCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxIndexedTreeNodeLeafLevel.Insert(ARelativeIndex: Integer; AItem: TdxCustomParagraph): TdxIndexedTreeNodeBase;
var
  AHalfCount, ATargetIndex: Integer;
  ARightNode: TdxIndexedTreeNodeLeafLevel;
begin
  if ChildrenCount < MaxChildrenCount then
  begin
    InsertWithShiftIndex(ARelativeIndex, AItem);
    Result := nil;
    Exit;
  end;
  AHalfCount := MaxChildrenCount div 2;
  ARightNode := SplitNode;
  if (ARelativeIndex > AHalfCount) then
  begin
    ATargetIndex := ARelativeIndex - AHalfCount;
    ARightNode.InsertWithShiftIndex(ATargetIndex, AItem);
  end
  else
  begin
    InsertWithShiftIndex(ARelativeIndex, AItem);
    ARightNode.FirstRelativeIndex := ARightNode.FirstRelativeIndex + 1;
  end;
  Result := ARightNode;
end;

procedure TdxIndexedTreeNodeLeafLevel.InsertWithShiftIndex(ATargetIndex: Integer; AItem: TdxCustomParagraph);
var
  I: Integer;
begin
  for I := ATargetIndex to ChildrenCount - 1 do
    TdxCustomParagraph(Items[I]).ShiftRelativeIndex(1);
  Items.Insert(ATargetIndex, AItem);

  AItem.Parent := Self;
  AItem.RelativeIndex := ATargetIndex;
end;

function TdxIndexedTreeNodeLeafLevel.Last: TdxCustomParagraph;
begin
  if FItems.Count <= 0 then
    Result := nil
  else
    Result := FItems[FItems.Count - 1];
end;

procedure TdxIndexedTreeNodeLeafLevel.OnEndMultipleRunSplit;
var
  ARelativeLogPosition, ARelativeFirstRunIndex: Integer;
begin
  ARelativeFirstRunIndex := Items[0].RelativeFirstRunIndex;
  if ARelativeFirstRunIndex <> 0 then
    ShiftRelativeFirstRunIndex(ARelativeFirstRunIndex);
  ARelativeLogPosition := Items[0].RelativeLogPosition;
  if ARelativeLogPosition <> 0 then
    ShiftRelativeLogPosition(ARelativeLogPosition);
end;

procedure TdxIndexedTreeNodeLeafLevel.RecalcParagraphsPositionsCore(AFrom, ATo, ATotalDescendantCount, ADeltaLength, ADeltaRunIndex: Integer);

  procedure Error;
  begin
    TdxRichEditExceptions.ThrowInternalException;
  end;

var
  I: Integer;
  ACount: Integer;
begin
  ACount := FItems.Count;
  if (AFrom <= 0) and (ATo >= ATotalDescendantCount - 1) and (Items[0].RelativeFirstRunIndex = 0)  then
  begin
    RelativeFirstRunIndex := RelativeFirstRunIndex + ADeltaRunIndex;
    if RelativeFirstRunIndex < 0 then
      ShiftRelativeFirstRunIndex(RelativeFirstRunIndex);
    RelativeLogPosition := RelativeLogPosition + ADeltaLength;
    if RelativeLogPosition < 0 then
      ShiftRelativeLogPosition(RelativeLogPosition);
    RelativeLastRunIndex := RelativeLastRunIndex + ADeltaRunIndex;
    Exit;
  end;
  AFrom := Max(AFrom, 0);
  ATo := Min(ATo, ACount - 1);
  for I := AFrom to ATo do
    TdxCustomParagraph(Items[I]).ShiftRelativePosition(ADeltaLength, ADeltaRunIndex, ADeltaRunIndex);
end;

function TdxIndexedTreeNodeLeafLevel.RemoveAt(ARelativeIndex: Integer): Boolean;
var
  I, AFirstRunIndex, AFirstLogPosition: Integer;
  AFirstItem: TdxCustomParagraph;
begin
  Result := False;
  Items[ARelativeIndex].Parent := nil;
  if FItems.Count = 1 then
  begin
    Assert(ARelativeIndex = 0);
    Result := True;
    Exit;
  end;
  for I := ARelativeIndex + 1 to FItems.Count - 1 do
    TdxCustomParagraph(Items[I]).ShiftRelativeIndex(-1);

  FItems[ARelativeIndex].Parent := nil;
  FItems.ExtractByIndex(ARelativeIndex);

  if ARelativeIndex = 0 then
  begin
    AFirstItem := Items[0];
    AFirstRunIndex := AFirstItem.RelativeFirstRunIndex;
    if AFirstRunIndex > 0 then
      ShiftRelativeFirstRunIndex(AFirstRunIndex);
    AFirstLogPosition := AFirstItem.RelativeLogPosition;
    if AFirstLogPosition > 0 then
      ShiftRelativeLogPosition(AFirstLogPosition);
  end;
end;

procedure TdxIndexedTreeNodeLeafLevel.ShiftRelativeFirstRunIndex(ADeltaRunIndex: Integer);
var
  I: Integer;
begin
  RelativeFirstRunIndex := RelativeFirstRunIndex + ADeltaRunIndex;
  for I := 0 to FItems.Count - 1 do
    TdxCustomParagraph(Items[I]).ShiftRelativePosition(0, -ADeltaRunIndex, 0);
  if (Parent <> nil) and ((RelativeFirstRunIndex < 0) or (TdxIndexedTreeNodeMiddleLevel(Parent).Children[0] = Self) and (RelativeFirstRunIndex > 0)) then
    TdxIndexedTreeNodeMiddleLevel(Parent).ShiftRelativeFirstRunIndex(RelativeFirstRunIndex);
end;

procedure TdxIndexedTreeNodeLeafLevel.ShiftRelativeLogPosition(ADeltaLogPosition: Integer);
var
  I: Integer;
begin
  RelativeLogPosition := RelativeLogPosition + ADeltaLogPosition;
  for I := 0 to FItems.Count - 1 do
    TdxCustomParagraph(Items[I]).ShiftRelativePosition(-ADeltaLogPosition, 0, 0);
  if (Parent <> nil) and ((RelativeLogPosition < 0) or (TdxIndexedTreeNodeMiddleLevel(Parent).Children[0] = Self) and (RelativeLogPosition > 0)) then
    TdxIndexedTreeNodeMiddleLevel(Parent).ShiftRelativeLogPosition(RelativeLogPosition);
end;

function TdxIndexedTreeNodeLeafLevel.SplitNode: TdxIndexedTreeNodeLeafLevel;
var
  I, AHalfCount, ADeltaFirstRunIndex, ADeltaLastRunIndex, ADeltaLogPosition: Integer;
  AItem: TdxCustomParagraph;
begin
  Result := CreateNew;
  AHalfCount := MaxChildrenCount div 2;
  ADeltaFirstRunIndex := Items[AHalfCount].RelativeFirstRunIndex;
  ADeltaLastRunIndex := Items[AHalfCount].RelativeLastRunIndex;
  ADeltaLogPosition := Items[AHalfCount].RelativeLogPosition;

  Result.RelativeFirstRunIndex := RelativeFirstRunIndex + ADeltaFirstRunIndex;
  Result.RelativeLastRunIndex := RelativeLastRunIndex + ADeltaLastRunIndex;
  Result.RelativeLogPosition := RelativeLogPosition + ADeltaLogPosition;
  for I := AHalfCount to MaxChildrenCount - 1 do
  begin
    AItem := Items[I];
    Result.FItems.Add(AItem);
    AItem.RelativeIndex := I - AHalfCount;
    AItem.Parent := Result;
    AItem.ShiftRelativePosition(-ADeltaLogPosition, -ADeltaFirstRunIndex, -ADeltaLastRunIndex);
  end;
  FItems.ExtractRange(AHalfCount, MaxChildrenCount - AHalfCount);
  Result.FirstRelativeIndex := FirstRelativeIndex + AHalfCount;
end;

function TdxIndexedTreeNodeLeafLevel.ToString: string;
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := 0 to ChildrenCount - 1 do
    if I = 0 then
      S := Format('%d,%d', [Items[I].RelativeFirstRunIndex, Items[I].RelativeLogPosition])
    else
      S := S + ',' + Format('%d,%d', [Items[I].RelativeFirstRunIndex, Items[I].RelativeLogPosition]);
  Result := Format('+%d,%d,%d:(%s)', [FirstRelativeIndex, RelativeFirstRunIndex, RelativeLogPosition, S]);
end;

{ TdxIndexedTree }

constructor TdxIndexedTree.Create(AMaxChildrenCount: Integer = DefaultMaxChildrenCount);
begin
  inherited Create;
  FMaxChildrenCount := AMaxChildrenCount;
  Clear;
end;

destructor TdxIndexedTree.Destroy;
begin
  FreeAndNil(FRoot);
  inherited Destroy;
end;

function TdxIndexedTree.CreateLeafLevel: TdxIndexedTreeNodeLeafLevel;
begin
  Result := TdxIndexedTreeNodeLeafLevel.Create(MaxChildrenCount);
end;

function TdxIndexedTree.CreateMiddleLevel: TdxIndexedTreeNodeMiddleLevel;
begin
  Result := TdxIndexedTreeNodeMiddleLevel.Create(MaxChildrenCount);
end;

function TdxIndexedTree.FindItemByIndex(AIndex: TdxParagraphIndex): TdxCustomParagraph;
begin
  if AIndex < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Index', AIndex);
  Result := FRoot.FindItemByIndex(AIndex);
end;

function TdxIndexedTree.First: TdxCustomParagraph;
begin
  Result := FRoot.First;
end;

function TdxIndexedTree.GetFirstCore: TdxParagraphBase;
begin
  Result := First;
end;

function TdxIndexedTree.GetLastCore: TdxParagraphBase;
begin
  Result := Last;
end;

function TdxIndexedTree.GetCount: Integer;
begin
  Result := FCount;
end;

function TdxIndexedTree.GetItemBase(Index: Integer): TdxParagraphBase;
begin
  Result := GetItem(Index);
end;

function TdxIndexedTree.GetItem(Index: Integer): TdxCustomParagraph;
begin
  Result := FindItemByIndex(Index);
end;

function TdxIndexedTree.IndexOf(AItem: TdxCustomParagraph): TdxParagraphIndex;
begin
  for Result := 0 to Count - 1 do
    if Items[Result] = AItem then
      Exit;
  Result := -1;
end;

procedure TdxIndexedTree.Insert(AIndex: TdxParagraphIndex; AItem: TdxCustomParagraph);
begin
  InsertCore(AIndex, AItem);
end;

procedure TdxIndexedTree.InsertCore(AIndex: Integer; AItem: TdxCustomParagraph);
var
  ANewChild: TdxIndexedTreeNodeBase;
  ANewRoot: TdxIndexedTreeNodeMiddleLevel;
begin
  Inc(FCount);

  ANewChild := FRoot.Insert(AIndex, AItem);
  if ANewChild = nil then
    Exit;
  ANewRoot := CreateMiddleLevel;
  ANewRoot.AddChild(FRoot);
  ANewRoot.AddChild(ANewChild);
  FRoot := ANewRoot;
end;

function TdxIndexedTree.Last: TdxCustomParagraph;
begin
  Result := FRoot.Last;
end;

procedure TdxIndexedTree.RemoveAt(ARelativeIndex: TdxParagraphIndex);
var
  AShouldClear: Boolean;
begin
  Dec(FCount);
  AShouldClear := FRoot.RemoveAt(ARelativeIndex);
  if not AShouldClear then
  begin
    Exit;
  end;
  FRoot := CreateLeafLevel;
end;

procedure TdxIndexedTree.Add(AItem: TdxCustomParagraph);
begin
  InsertCore(Count, AItem);
end;


procedure TdxIndexedTree.Clear;
begin
  FreeAndNil(FRoot);
  FRoot := CreateLeafLevel;
  FCount := 0;
end;

procedure TdxIndexedTree.ForEach(const AAction: TdxAction<TdxCustomParagraph>);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AAction(Self[I]);
end;

procedure TdxIndexedTree.AddRange(const ACollection: TdxList<TdxCustomParagraph>);
var
  AParagraph: TdxCustomParagraph;
  I: Integer;
begin
  for I := 0 to ACollection.Count - 1 do
  begin
    AParagraph := ACollection[I];
    Add(AParagraph);
  end;
end;

function TdxIndexedTree.GetRange(AIndex, ACount: Integer): TdxList<TdxCustomParagraph>;
var
  I, AEndIndex: Integer;
begin
  Result := TdxList<TdxCustomParagraph>.Create;
  AEndIndex := AIndex + ACount - 1;
  for I := AIndex to AEndIndex do
    Result.Add(FRoot.FindItemByIndex(I));
end;


{ TdxParagraphIndexedTree }


procedure TdxParagraphIndexedTree.RecalcParagraphsPositionsCore(AFrom, ATo: TdxParagraphIndex; ADeltaLength,
  ADeltaRunIndex: Integer);
begin
  if ATo < AFrom then
    Exit;
  Root.RecalcParagraphsPositionsCore(AFrom, ATo, Count, ADeltaLength, ADeltaRunIndex);
end;

function TdxParagraphIndexedTree.SearchByLogPosition(ALogPosition: TdxDocumentLogPosition): TdxParagraphIndex;
var
  APosition, I: Integer;
  ACurrent: TdxIndexedTreeNodeBase;
  AIsLastItem: Boolean;
  ACurrentMiddleLevel: TdxIndexedTreeNodeMiddleLevel;
  ACurrentLeafLevel: TdxIndexedTreeNodeLeafLevel;
  AItems: TdxCustomParagraphList;
  AParagraph: TdxCustomParagraph;
  AChild: TdxIndexedTreeNodeBase;
begin
  if ALogPosition < 0 then
    Exit(-1);

  APosition := ALogPosition;
  ACurrent := Root;
  AIsLastItem := True;
  while True do
  begin
    if ACurrent is TdxIndexedTreeNodeMiddleLevel then
    begin
      ACurrentMiddleLevel := TdxIndexedTreeNodeMiddleLevel(ACurrent);
      for I := ACurrentMiddleLevel.ChildrenCount - 1 downto 0 do
      begin
        AChild := ACurrentMiddleLevel.Children[I];
        if APosition >= AChild.RelativeLogPosition then
        begin
          APosition := APosition - AChild.RelativeLogPosition;
          ACurrent := AChild;
          Break;
        end;
        AIsLastItem := false;
      end;
      Assert(I >= 0);
    end
    else
    begin
      Assert(ACurrent is TdxIndexedTreeNodeLeafLevel);
      ACurrentLeafLevel := TdxIndexedTreeNodeLeafLevel(ACurrent);
      AItems := ACurrentLeafLevel.Items;
      for I := AItems.Count - 1 downto 0 do
      begin
        AParagraph := AItems[I];
        if APosition >= AParagraph.RelativeLogPosition then
        begin
          if AIsLastItem and (ALogPosition > AParagraph.EndLogPosition) then
            Exit(not Count);

          Exit(AParagraph.Index);
        end;
        AIsLastItem := False;
      end;
      Assert(False);
    end;
  end;
end;

{ TdxCustomParagraphCollection }

procedure TdxCustomParagraphCollection.OnBeginMultipleRunSplit;
begin
end;

procedure TdxCustomParagraphCollection.OnEndMultipleRunSplit;
begin
  if Root <> nil then
    Root.OnEndMultipleRunSplit;
end;

end.
