{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2010 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnTree;
{* |<PRE>
================================================================================
* ������ƣ�CnPack ������Ԫ
* ��Ԫ���ƣ�ʵ�ֵ������������൥Ԫ
* ��Ԫ���ߣ���Х (liuxiao@cnpack.org)
* ��    ע���õ�ԪΪ TCnTree �� TCnLeaf �ĵ�����������ʵ�ֵ�Ԫ��
*           ������ TTreeNodes �� TTreeNode �Ĺ�ϵ��֧����Ⱥ͹�����ȱ�����
*           ֧�ְ�������ȵ�˳��������ֵ����ʽֱ�ӷ��ʸ����ڵ㡣
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: CnTree.pas 418 2010-02-08 04:53:54Z zhoujingyu $
* �޸ļ�¼��2005.05.08 V1.3 by Alan
*               ���� LoadFromTreeView �������� Clear ����δ���� RootLeaf �����Ĵ���
*           2004.11.02 V1.2
*               ���������Ľӿ�
*           2004.09.04 V1.1
*               ����� TreeView �����Ĺ���
*           2004.05.29 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs, ComCtrls;

type
  ECnTreeException = class(Exception);

  TCnTree = class;

  TCnLeaf = class(TPersistent)
  private
    FData: Integer;
    FList: TList;
    FParent: TCnLeaf;
    FText: string;
    FTree: TCnTree;
    function GetAbsoluteIndex: Integer;
    function GetAllCount: Integer;
    function GetCount: Integer;
    function GetHasChildren: Boolean;
    function GetIndex: Integer;
    function GetItems(Index: Integer): TCnLeaf;
    function GetLevel: Integer;
  protected
    procedure DoDepthFirstTravel;
    procedure DoWidthFirstTravel;
    function SetChild(ALeaf: TCnLeaf; Index: Integer): TCnLeaf;
    {* ��ĳ�ڵ㸳ֵΪ�� Index ���ӽڵ㣬����ԭ�ڵ�ֵ }
  public
    constructor Create(ATree: TCnTree); virtual;
    {* ���췽������Ҫһ Tree ������ }
    destructor Destroy; override;
    {* �������� }
    function AddChild(ALeaf: TCnLeaf): TCnLeaf;
    {* ���һָ�����ӽڵ���Ϊ����ֱ���ӽڵ� }
    function AddChildFirst(ALeaf: TCnLeaf): TCnLeaf;
    {* ���һָ�����ӽڵ���Ϊ��һֱ���ӽڵ� }
    function InsertChild(ALeaf: TCnLeaf; AIndex: Integer): TCnLeaf;
    {* ��ָ����������������һ�ӽڵ� }
    procedure Clear;
    {* �������ֱ���ӽڵ㣬�ӽڵ�����Ҳ�ᱻɾ���ͷ�  }
    procedure DeleteChild(AIndex: Integer);
    {* ɾ��һֱ���ӽڵ㣬�ӽڵ����»ᱻɾ���ͷ� }
    procedure Delete;
    {* ɾ��������ӽڵ� }
    function ExtractChild(AIndex: Integer): TCnLeaf; overload;
    {* �����һֱ���ӽڵ㣬�ӽڵ����²��ᱻɾ���ͷ� }
    function ExtractChild(ALeaf: TCnLeaf): TCnLeaf; overload;
    {* �����ָ����һ�ӽڵ㣬�ӽڵ����²��ᱻɾ���ͷ� }

    // ��������ڵ�ķ���
    function GetFirstChild: TCnLeaf;
    {* ��õ�һ��ֱ���ӽڵ� }
    function GetLastChild: TCnLeaf;
    {* ������һ��ֱ���ӽڵ� }
    function GetNext: TCnLeaf;
    {* ��õ�һ�ӽڵ㣬���ޣ��򷵻�ͬ���ڵ�ĺ�һ���ڵ㣬���ޣ��򷵻� nil }
    function GetNextChild(Value: TCnLeaf): TCnLeaf;
    {* ���ĳ�ӽڵ�ĺ�һͬ���ڵ㣬���򷵻� nil }
    function GetNextSibling: TCnLeaf;
    {* ���ͬ���ĺ�һ�ӽڵ㣬���򷵻� nil }
    function GetPrev: TCnLeaf;
    {* ���ͬ���ڵ��ǰһ���ڵ㣬���ޣ��򷵻ظ��ڵ㣬���ޣ��򷵻� nil }
    function GetPrevChild(Value: TCnLeaf): TCnLeaf;
    {* ���ĳһ�ӽڵ��ǰһͬ���ڵ㣬���򷵻� nil }
    function GetPrevSibling: TCnLeaf;
    {* ���ͬ����ǰһ�ӽڵ㣬���򷵻� nil }
    function GetAbsoluteItems(AAbsoluteIndex: Integer): TCnLeaf;
    {* ����������ȵı���˳���õ� n ���ӽڵ㣬������ TreeNodes �еĻ��� }

    function HasAsParent(Value: TCnLeaf): Boolean;
    {* ָ���Ľڵ��Ƿ��Ǳ��ڵ���ϼ�����ϼ� }
    function IndexOf(ALeaf: TCnLeaf): Integer;
    {* ��ֱ���ӽڵ��в����Ƿ���ĳһ�ڵ㲢���������� }
    property AbsoluteIndex: Integer read GetAbsoluteIndex;
    {* ���������е�����ֵ }
    property AllCount: Integer read GetAllCount;
    {* ��������ڵ���Ŀ }
    property Count: Integer read GetCount;
    {* ֱ���ӽڵ���Ŀ }
    property HasChildren: Boolean read GetHasChildren;
    {* �Ƿ����ӽڵ� }
    property Index: Integer read GetIndex;
    {* ��Ҷ�ڵ��ڸ��ڵ��б��е�˳���������� 0 ��ʼ���޸���Ϊ -1 }
    property Items[Index: Integer]: TCnLeaf read GetItems; default;
    {* ֱ��Ҷ�ڵ����� }

    property Level: Integer read GetLevel;
    {* ���ڵ�����������ڵ� Level Ϊ 1 }
    property Parent: TCnLeaf read FParent;
    {* ���ڵ㣬����д }
    property Tree: TCnTree read FTree;
    {* ��������һ��Ҷ��������һ���� }
  published
    property Data: Integer read FData write FData;
    {* ���Ա���һ���������ԣ������� Tag }
    property Text: string read FText write FText;
    {* ���Ա���һ�ַ��������� }
  end;

  ICnTreeFiler = interface(IUnknown)
  {* �����������Ľӿ� }
    ['{E81A9CE0-2D1D-11D9-BA1C-5254AB35836A}']
    procedure LoadFromFile(Instance: TPersistent; const FileName: string);
    procedure SaveToFile(Instance: TPersistent; const FileName: string);
  end;

  TCnLeafClass = class of TCnLeaf;

  TCnTreeNodeEvent = procedure(ALeaf: TCnLeaf; ATreeNode: TTreeNode;
    var Valid: Boolean) of object;

  TCnTree = class(TPersistent)
  private
    FLeafClass: TCnLeafClass;
    FBatchUpdating: Boolean;
    FLeaves: TObjectList;
    FRoot: TCnLeaf;
    FOnWFTLeaf: TNotifyEvent;
    FOnDFTLeaf: TNotifyEvent;
    FOnSaveANode: TCnTreeNodeEvent;
    FOnLoadANode: TCnTreeNodeEvent;
    function GetRoot: TCnLeaf;
    function GetItems(AbsoluteIndex: Integer): TCnLeaf;
    function GetCount: Integer;
    function GetRegisteredCount: Integer;
  protected
    function CreateLeaf(ATree: TCnTree): TCnLeaf; virtual;
    procedure DoDFTLeaf(ALeaf: TCnLeaf); virtual;
    procedure DoWFTLeaf(ALeaf: TCnLeaf); virtual;
    function DoLoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean; virtual;
    function DoSaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean; virtual;

    procedure RegisterLeaf(ALeaf: TCnLeaf);
    {* ����Ҷ�ڵ���ã������еǼǴ�Ҷ�ڵ� }
    procedure UnRegisterLeaf(ALeaf: TCnLeaf);
    {* ����Ҷ�ڵ���ã�ȡ����Ҷ�ڵ�ĵǼ� }

    procedure LoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); virtual;
    {* ��һ TreeNode �ڵ��������ӽڵ㣬���ݹ���� }
    procedure SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); virtual;
    {* ���ڵ㱾���Լ��ӽڵ�д��һ TreeNode�����ݹ���� }
  public
    constructor Create; overload;
    {* ���췽�� }
    constructor Create(LeafClass: TCnLeafClass); overload;
    {* ��һ���췽��}
    destructor Destroy; override;
    {* �������� }
    procedure DepthFirstTravel;
    {* ����������ȱ��� }
    procedure WidthFirstTravel;
    {* ���й�����ȱ��� }
    function ExtractLeaf(ALeaf: TCnLeaf): TCnLeaf;
    {* �����а���һҶ�ڵ㲢������ }
    procedure Clear;
    {* ������ͷ�����Ҷ�ڵ㣬��������ͷţ���������������������֪ͨ���� }

    // ������ӷ���
    function AddChildFirst(AParent: TCnLeaf): TCnLeaf;
    {* ��ָ���Ľڵ�����һ���ӽڵ� }
    function AddChild(AParent: TCnLeaf): TCnLeaf;
    {* ��ָ���Ľڵ�����һβ�ӽڵ� }
    function InsertChild(AParent: TCnLeaf; AIndex: Integer): TCnLeaf;
    {* ��ָ���Ľڵ�����һָ��λ�õ��ӽڵ� }
    function AddFirst(ASibing: TCnLeaf): TCnLeaf;
    {* ��ָ���Ľڵ�����һͬ������ǰ�ڵ� }
    function Add(ASibing: TCnLeaf): TCnLeaf;
    {* ��ָ���Ľڵ�����һͬ�������ڵ� }

    procedure ExchangeWithChild(Leaf1, Leaf2: TCnLeaf); overload;
    {* �������ڵ�λ�ã����ӽڵ���һ�𽻻� }
    procedure ExchangeWithChild(AbsoluteIndex1, AbsoluteIndex2: Integer); overload;
    {* �������ڵ�λ�ã����ӽڵ���һ�𽻻� }
    procedure Exchange(Leaf1, Leaf2: TCnLeaf); overload;
    {* �����������ڵ�λ�� }
    procedure Exchange(AbsoluteIndex1, AbsoluteIndex2: Integer); overload;
    {* �������������������ڵ�λ�� }

    // �� TreeView �Ľ���������ע�� Root �����뽻��
    procedure LoadFromTreeView(ATreeView: TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnLeaf = nil);
    {* ��һ TreeView ����ڵ����ݡ�RootNode ���ӽڵ㱻����� RootLeaf ��ָ����
    �ڵ���ӽڵ㣬RootNode Ϊ nil ��ʾ����ȫ�� TreeNodes��RootLeaf Ϊ nil ��ʾ
    �����Ϊ Tree.Root ��ֱ���ڵ㣬Ҳ�������нڵ�}
    procedure SaveToTreeView(ATreeView: TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnLeaf = nil);
    {* ���ڵ�����д��һ TreeView�� RootLeaf ���ӽڵ㱻д��� RootNode ��ָ����
    �ڵ���ӽڵ㣬RootLeaf Ϊ nil ��ʾд�� Root �������ӽڵ㣬��ʵҲ�������н�
    �㣬RootNode Ϊ nil ��ʾд��Ľ���Ϊ TreeView �ĸ� TreeNodes}

    // ��������
    procedure LoadFromFile(Filer: ICnTreeFiler; const FileName: string); virtual;
    {* ���ļ����������ڵ㣬���ṩ�ӿڵĶ���ʵ�� }
    procedure SaveToFile(Filer: ICnTreeFiler; const FileName: string); virtual;
    {* �����ڵ㱣�����ļ������ṩ�ӿڵĶ���ʵ�� }

    property BatchUpdating: Boolean read FBatchUpdating write FBatchUpdating;
    {* �Ƿ����������£�Ϊ True ʱҶ�ڵ��ͷ�ʱ��֪ͨ Tree }
    property Root: TCnLeaf read GetRoot;
    {* ���ڵ㣬���Ǵ��� }
    property Items[AbsoluteIndex: Integer]: TCnLeaf read GetItems;
    {* ����������ȵı���˳���õ� n ���ӽڵ㣬������ TreeNodes �еĻ��� }
    property Count: Integer read GetCount;
    {* �������������ӽڵ����Ŀ }
    property RegisteredCount: Integer read GetRegisteredCount;
    {* ������������ע������ӽڵ����Ŀ }
  published
    property OnDFTLeaf: TNotifyEvent read FOnDFTLeaf write FOnDFTLeaf;
    {* ������ȱ���ʱ������һ��Ҷ�ڵ�ʱ�Ĵ����¼���Sender �Ǵ˽ڵ� }
    property OnWFTLeaf: TNotifyEvent read FOnWFTLeaf write FOnWFTLeaf;
    {* ������ȱ���ʱ������һ��Ҷ�ڵ�ʱ�Ĵ����¼���Sender �Ǵ˽ڵ� }
    property OnLoadANode: TCnTreeNodeEvent read FOnLoadANode write FOnLoadANode;
    {* �� TreeView ������ڵ�ʱ���ÿһ���ڵ�Ĵ����¼� }
    property OnSaveANode: TCnTreeNodeEvent read FOnSaveANode write FOnSaveANode;
    {* ���ڵ���� TreeView ʱ���ÿһ���ڵ�Ĵ����¼� }
  end;

implementation

//==============================================================================
// TCnLeaf
//==============================================================================

constructor TCnLeaf.Create(ATree: TCnTree);
begin
  inherited Create;
  Assert(ATree <> nil);
  FList := TList.Create;
  FTree := ATree;
  ATree.RegisterLeaf(Self);
end;

destructor TCnLeaf.Destroy;
var
  I: Integer;
begin
  if not FTree.BatchUpdating then
  begin
    for I := FList.Count - 1 downto 0 do
      DeleteChild(I);
    FTree.UnregisterLeaf(Self);
  end;
  FreeAndNil(FList);
  inherited;
end;

function TCnLeaf.AddChild(ALeaf: TCnLeaf): TCnLeaf;
begin
  Assert(ALeaf.Tree = Self.FTree);
  Result := ALeaf;
  FList.Add(Result);
  Result.FParent := Self;
end;

function TCnLeaf.AddChildFirst(ALeaf: TCnLeaf): TCnLeaf;
begin
  Assert(ALeaf.Tree = Self.FTree);
  Result := ALeaf;
  FList.Insert(0, Result);
  Result.FParent := Self;
end;

procedure TCnLeaf.Clear;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
  begin
    TCnLeaf(FList.Items[I]).Free;
    FList.Delete(I);
  end;
end;

procedure TCnLeaf.DeleteChild(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    TCnLeaf(FList.Items[AIndex]).Free;
    FList.Delete(AIndex);
  end;
end;

procedure TCnLeaf.Delete;
begin
  if FParent <> nil then
    FParent.DeleteChild(Index)
  else
    raise ECnTreeException.Create('Root can NOT be deleted.');
end;

function TCnLeaf.ExtractChild(ALeaf: TCnLeaf): TCnLeaf;
var
  AIndex: Integer;
begin
  if ALeaf.HasAsParent(Self) then
  begin
    AIndex := ALeaf.Index;
    Result := ALeaf.Parent.Items[AIndex];
    ALeaf.Parent.FList.Delete(AIndex);
  end
  else
    Result := nil;
end;

function TCnLeaf.ExtractChild(AIndex: Integer): TCnLeaf; 
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    Result := TCnLeaf(Items[AIndex]);
    Result.FParent := nil;
    FList.Delete(AIndex);
  end;
end;

procedure TCnLeaf.DoDepthFirstTravel;
var
  I: Integer;
begin
  if FTree <> nil then
    FTree.DoDFTLeaf(Self);
  for I := 0 to FList.Count - 1 do
    Items[I].DoDepthFirstTravel;
end;

procedure TCnLeaf.DoWidthFirstTravel;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    FTree.DoWFTLeaf(TCnLeaf(Items[I]));
  for I := 0 to FList.Count - 1 do
    Items[I].DoWidthFirstTravel;
end;

function TCnLeaf.GetAbsoluteIndex: Integer;
begin
  if FParent <> nil then
    Result := Self.Index + FParent.AbsoluteIndex + 1
  else
    Result := 0;
end;

function TCnLeaf.GetAbsoluteItems(AAbsoluteIndex: Integer): TCnLeaf;
var
  I, ACount, IndexCount: Integer;
begin
  Result := nil;
  if AAbsoluteIndex < 0 then
    Exit
  else
  begin
    IndexCount := 0;
    for I := 0 to Count - 1 do
    begin
      if IndexCount = AAbsoluteIndex then
      begin
        Result := Items[I];
        Exit;
      end;

      ACount := Items[I].AllCount + 1;
      if IndexCount + ACount > AAbsoluteIndex then
      begin
        Result := Items[I].GetAbsoluteItems(AAbsoluteIndex - IndexCount - 1);
        Exit;
      end
      else
        Inc(IndexCount, ACount);
    end;
  end;
end;

function TCnLeaf.GetAllCount: Integer;
var
  I: Integer;
begin
  Result := Count;
  for I := 0 to Self.Count - 1 do
    Result := Result + Self.Items[I].AllCount;
end;

function TCnLeaf.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCnLeaf.GetFirstChild: TCnLeaf;
begin
  if HasChildren then
    Result := TCnLeaf(FList.Items[0])
  else
    Result := nil;
end;

function TCnLeaf.GetHasChildren: Boolean;
begin
  Result := FList.Count > 0;
end;

function TCnLeaf.GetIndex: Integer;
begin
  if FParent <> nil then
    Result := FParent.IndexOf(Self)
  else
    Result := -1;
end;

function TCnLeaf.GetItems(Index: Integer): TCnLeaf;
begin
  Result := TCnLeaf(FList.Items[Index]);
end;

function TCnLeaf.GetLastChild: TCnLeaf;
begin
  if HasChildren then
    Result := TCnLeaf(FList.Items[Count - 1])
  else
    Result := nil;
end;

function TCnLeaf.GetLevel: Integer;
begin
  if FParent = nil then
    Result := 1
  else
    Result := FParent.Level + 1;
end;

function TCnLeaf.HasAsParent(Value: TCnLeaf): Boolean;
var
  AParent: TCnLeaf;
begin
  Result := False;
  if Value.Tree <> Self.Tree then
    Exit;
    
  AParent := FParent;
  while AParent <> nil do
  begin
    if AParent = Value then
    begin
      Result := True;
      Exit;
    end
    else
      AParent := AParent.Parent;
  end;
end;

function TCnLeaf.IndexOf(ALeaf: TCnLeaf): Integer;
begin
  Result := FList.IndexOf(ALeaf);
end;

function TCnLeaf.GetNext: TCnLeaf;
begin
  Result := GetFirstChild;
  if Result = nil then
    Result := GetNextSibling;
end;

function TCnLeaf.GetNextChild(Value: TCnLeaf): TCnLeaf;
begin
  Result := nil;
  if Value.Parent = Self then
    if Value.Index < Self.Count - 1 then
      Result := Items[Value.Index + 1];
end;

function TCnLeaf.GetNextSibling: TCnLeaf;
begin
  Result := nil;
  if Parent <> nil then
    if Index < Parent.Count - 1 then
      Result := Parent.Items[Index + 1];
end;

function TCnLeaf.GetPrev: TCnLeaf;
begin
  Result := GetPrevSibling;
  if Result = nil then
    Result := Parent;
end;

function TCnLeaf.GetPrevChild(Value: TCnLeaf): TCnLeaf;
begin
  Result := nil;
  if Value.Parent = Self then
    if Value.Index > 0 then
      Result := Items[Value.Index - 1];
end;

function TCnLeaf.GetPrevSibling: TCnLeaf;
begin
  Result := nil;
  if Parent <> nil then
    if Index > 0 then
      Result := Parent.Items[Index - 1];
end;

function TCnLeaf.SetChild(ALeaf: TCnLeaf; Index: Integer): TCnLeaf;
begin
  if (ALeaf <> nil) and (ALeaf.Tree = Self.FTree) and
    (Index >= 0) and (Index < Count) then
  begin
    Result := FList.Items[Index];
    FList.Items[Index] := ALeaf;
    ALeaf.FParent := Self;
  end
  else
    Result := nil;
end;

//==============================================================================
// TCnTree
//==============================================================================

constructor TCnTree.Create;
begin
  inherited;
  FLeaves := TObjectList.Create(True);
  if FLeafClass = nil then
    FLeafClass := TCnLeaf;
  FRoot := CreateLeaf(Self);
end;

constructor TCnTree.Create(LeafClass: TCnLeafClass);
begin
  FLeafClass := LeafClass;
  Create;
end;

destructor TCnTree.Destroy;
begin
  FBatchUpdating := True;
  FLeaves.Free;
  inherited;
end;

procedure TCnTree.DepthFirstTravel;
begin
  FRoot.DoDepthFirstTravel;
end;

function TCnTree.CreateLeaf(ATree: TCnTree): TCnLeaf;
begin
  try
    Result := TCnLeaf(FLeafClass.NewInstance);
    Result.Create(ATree);
  except
    Result := nil;
  end;
end;

procedure TCnTree.DoDFTLeaf(ALeaf: TCnLeaf);
begin
  if Assigned(FOnDFTLeaf) then
    FOnDFTLeaf(ALeaf);
end;

procedure TCnTree.DoWFTLeaf(ALeaf: TCnLeaf);
begin
  if Assigned(FOnWFTLeaf) then
    FOnWFTLeaf(ALeaf);
end;

function TCnTree.GetRoot: TCnLeaf;
begin
  Result := FRoot;
end;

procedure TCnTree.RegisterLeaf(ALeaf: TCnLeaf);
begin
  if FLeaves.IndexOf(ALeaf) < 0 then
    FLeaves.Add(ALeaf);
end;

procedure TCnTree.WidthFirstTravel;
begin
  DoWFTLeaf(FRoot);
  FRoot.DoWidthFirstTravel;
end;

procedure TCnTree.UnRegisterLeaf(ALeaf: TCnLeaf);
begin
  FLeaves.Extract(ALeaf);
end;

procedure TCnTree.Clear;
begin
  FBatchUpdating := True;
  try
    FLeaves.Clear;
    // FRoot �Ѿ��� Fleaves �ͷţ������ٴ��ͷ�.
    FRoot := CreateLeaf(Self);
  finally
    FBatchUpdating := False;
  end;
end;

function TCnTree.ExtractLeaf(ALeaf: TCnLeaf): TCnLeaf;
begin
  Result := nil;
  if ALeaf.Tree = Self then
  begin
    Self.UnRegisterLeaf(ALeaf);
    if ALeaf.Parent <> nil then
      Result := ALeaf.Parent.ExtractChild(ALeaf.Index);
  end;
end;

function TCnLeaf.InsertChild(ALeaf: TCnLeaf; AIndex: Integer): TCnLeaf;
begin
  if (ALeaf <> nil) and (AIndex >= 0) and (AIndex <= Count) then
  begin
    Result := ALeaf;
    FList.Insert(AIndex, ALeaf);
    ALeaf.FParent := Self;
  end
  else
    Result := nil;
end;

function TCnTree.AddChild(AParent: TCnLeaf): TCnLeaf;
begin
  if AParent.Tree = Self then
  begin
    Result := CreateLeaf(Self);
    AParent.AddChild(Result);
  end
  else
    Result := nil;
end;

function TCnTree.AddChildFirst(AParent: TCnLeaf): TCnLeaf;
begin
  if AParent.Tree = Self then
  begin
    Result := CreateLeaf(Self);
    AParent.AddChildFirst(Result);
  end
  else
    Result := nil;
end;

function TCnTree.InsertChild(AParent: TCnLeaf; AIndex: Integer): TCnLeaf;
begin
  if AParent.Tree = Self then
  begin
    Result := CreateLeaf(Self);
    if AParent.InsertChild(Result, AIndex) = nil then
    begin
      Result.Free;
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

function TCnTree.AddFirst(ASibing: TCnLeaf): TCnLeaf;
begin
  if (ASibing <> nil) and (ASibing.Tree = Self) and (ASibing.Parent <> nil) then
  begin
    Result := CreateLeaf(Self);
    if ASibing.Parent.AddChildFirst(Result) = nil then
    begin
      Result.Free;
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

function TCnTree.Add(ASibing: TCnLeaf): TCnLeaf;
begin
  if (ASibing <> nil) and (ASibing.Tree = Self) and (ASibing.Parent <> nil) then
  begin
    Result := CreateLeaf(Self);
    if ASibing.Parent.AddChildFirst(Result) = nil then
    begin
      Result.Free;
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

procedure TCnTree.Exchange(AbsoluteIndex1, AbsoluteIndex2: Integer); 
begin
  Exchange(Items[AbsoluteIndex1], Items[AbsoluteIndex2]);
end;

procedure TCnTree.ExchangeWithChild(AbsoluteIndex1,
  AbsoluteIndex2: Integer);
begin
  ExchangeWithChild(Items[AbsoluteIndex1], Items[AbsoluteIndex2]);
end;

procedure TCnTree.ExchangeWithChild(Leaf1, Leaf2: TCnLeaf); 
var
  Parent2: TCnLeaf;
  Index2: Integer;
begin
  if (Leaf1 <> nil) and (Leaf2 <> nil) and (Leaf1 <> Leaf2)
    and (Leaf1.Tree = Self) and (Leaf2.Tree = Self) then
  begin
    if Leaf1.HasAsParent(Leaf2) or Leaf2.HasAsParent(Leaf1) then
      Exit; // Ϊ���ӹ�ϵ�Ĳ�������
    Parent2 := Leaf2.Parent;
    Index2 := Leaf2.Index;

    Leaf1.Parent.SetChild(Leaf2, Leaf1.Index);
    Parent2.SetChild(Leaf1, Index2);
  end;
end;

procedure TCnTree.Exchange(Leaf1, Leaf2: TCnLeaf); 
var
  Parent2: TCnLeaf;
  I, Index2: Integer;
  AList: TList;
begin
  if (Leaf1 <> nil) and (Leaf2 <> nil) and (Leaf1 <> Leaf2)
    and (Leaf1.Tree = Self) and (Leaf2.Tree = Self) then
  begin
    // ���������ڵ���ӽڵ��б����ڵ㽻��������
    Parent2 := Leaf2.Parent;
    Index2 := Leaf2.Index;

    AList := nil;
    try
      AList := TList.Create;
      for I := 0 to Leaf1.Count - 1 do
        AList.Add(Leaf1.Items[I]);

      Leaf1.FList.Clear;
      for I := 0 to Leaf2.Count - 1 do
        Leaf1.FList.Add(Leaf2.Items[I]);
      for I := 0 to AList.Count - 1 do
        Leaf2.FList.Add(AList.Items[I]);
    finally
      AList.Free;
    end;

    if Leaf1.Parent <> nil then
      Leaf1.Parent.SetChild(Leaf2, Leaf1.Index)
    else
      Leaf2.FParent := nil;
    if Parent2 <> nil then
      Parent2.SetChild(Leaf1, Index2)
    else
      Leaf1.FParent := nil;

    // ˳���жϸ��ڵ�
    if FRoot = Leaf1 then
      FRoot := Leaf2
    else if FRoot = Leaf2 then
      FRoot := Leaf1;
  end;
end;

function TCnTree.GetItems(AbsoluteIndex: Integer): TCnLeaf;
begin
  if AbsoluteIndex < 0 then
    Result := nil
  else if AbsoluteIndex = 0 then
    Result := FRoot
  else
    Result := FRoot.GetAbsoluteItems(AbsoluteIndex - 1);
end;

function TCnTree.GetCount: Integer;
begin
  Result := FRoot.AllCount + 1;
end;

function TCnTree.GetRegisteredCount: Integer;
begin
  Result := FLeaves.Count;
end;

procedure TCnTree.LoadFromTreeView(ATreeView: TTreeView; RootNode: TTreeNode;
  RootLeaf: TCnLeaf);
var
  ANode: TTreeNode;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootNode <> nil) and (RootNode.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    if RootLeaf = nil then
      Self.Clear
    else
      RootLeaf.Clear;
      
    if ATreeView.Items.Count > 0 then
    begin
      if RootNode = nil then
        ANode := ATreeView.Items[0]
      else
        ANode := RootNode;
      // ��һ���ڵ�
      if RootLeaf = nil then
        RootLeaf := FRoot;

      ALeaf := Self.AddChild(RootLeaf);
      LoadFromATreeNode(ALeaf, ANode);
      if RootNode <> nil then Exit;
      // ������ RootNode ʱ�� RootNode Ϊ�������Բ����� RootNode ��ͬ��ڵ�

      ANode := ANode.GetNextSibling; // �����˲��������̽ڵ�
      while ANode <> nil do
      begin
        ALeaf := Self.AddChild(RootLeaf);
        LoadFromATreeNode(ALeaf, ANode);
        ANode := ANode.GetNextSibling;
      end;
    end;
  end;
end;

procedure TCnTree.SaveToTreeView(ATreeView: TTreeView; RootNode: TTreeNode;
  RootLeaf: TCnLeaf);
var
  I: Integer;
  ANode: TTreeNode;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootNode <> nil) and (RootNode.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    ATreeView.Items.BeginUpdate;
    try
      if RootNode <> nil then
        RootNode.DeleteChildren
      else
        ATreeView.Items.Clear;

      if RootLeaf = nil then
        RootLeaf := Self.FRoot;
      if RootLeaf.Count > 0 then
      begin
        ANode := RootNode;
        for I := 0 to RootLeaf.Count - 1 do
        begin
          ALeaf := RootLeaf.Items[I]; // RootLeaf ���ӽڵ㣬RootLeaf �����뽻��
          ANode := ATreeView.Items.Add(ANode, '');
          SaveToATreeNode(ALeaf, ANode);
        end;
      end;
    finally
      ATreeView.Items.EndUpdate;
    end;
  end;
end;

procedure TCnTree.LoadFromFile(Filer: ICnTreeFiler;
  const FileName: string);
begin
  if Filer <> nil then
    Filer.LoadFromFile(Self, FileName);
end;

procedure TCnTree.SaveToFile(Filer: ICnTreeFiler; const FileName: string);
begin
  if Filer <> nil then
    Filer.SaveToFile(Self, FileName);
end;

procedure TCnTree.LoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode);
var
  I: Integer;
  Leaf: TCnLeaf;
begin
  if (ANode <> nil) and (ALeaf <> nil) then
  begin
    if DoLoadFromATreeNode(ALeaf, ANode) then
    begin
      for I := 0 to ANode.Count - 1 do
      begin
        Leaf := Self.AddChild(ALeaf);
        LoadFromATreeNode(Leaf, ANode.Item[I]);
      end;
    end
    else
    begin
      ALeaf.Delete;
    end;
  end;
end;

procedure TCnTree.SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode);
var
  I: Integer;
  Node: TTreeNode;
begin
  if (ANode <> nil) and (ALeaf <> nil) and (ANode.TreeView is TTreeView) then
  begin
    if DoSaveToATreeNode(ALeaf, ANode) then
    begin
      for I := 0 to ALeaf.Count - 1 do
      begin
        Node := (ANode.TreeView as TTreeView).Items.AddChild(ANode, '');
        SaveToATreeNode(ALeaf.Items[I], Node);
      end;
    end
    else
    begin
      ANode.Delete;
    end;
  end;
end;

function TCnTree.DoLoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean;
begin
  Result := True;
  if Assigned(FOnLoadANode) then
    FOnLoadANode(ALeaf, ANode, Result)
  else
  begin
    ALeaf.Text := ANode.Text;
    ALeaf.Data := Integer(ANode.Data);
  end;
end;

function TCnTree.DoSaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean;
begin
  Result := True;
  if Assigned(FOnSaveANode) then
  begin
    FOnSaveANode(ALeaf, ANode, Result);
  end
  else
  begin
    ANode.Text := ALeaf.Text;
    ANode.Data := Pointer(ALeaf.Data);
  end;
end;

end.
