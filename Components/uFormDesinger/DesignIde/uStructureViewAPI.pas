{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit uStructureViewAPI;

interface

uses Windows, CommCtrl, SysUtils, Classes, Menus, Controls, ActiveX, uToolsAPI;

const
  SourceCodeStructureType = 'SourceCode.StructureType';
  ErrorsNodeType = 'Errors.NodeType';
  DesignerStructureType = 'Designer.StructureType';

  // These view selection options mirror some corresponding options on the
  // Virtual tree-view used to implement the structure view

  voLevelSelectConstraint   = $0001; // Constrain selection to the same level as the selection anchor.
  voMultiSelect             = $0002; // Allow more than one node to be selected.
  voRightClickSelect        = $0004; // Allow selection, dragging etc. with the right mouse button.
  voSiblingSelectConstraint = $0008; // constrain selection to nodes with same parent

type
  {$MINENUMSIZE 4}
  TOTAChildUpdateKind = (ckNodeAdded, ckNodeRemoved, ckChanged);
  TOTADragState = (odsDragEnter, odsDragLeave, odsDragMove);
  TOTADropMode = (odmNowhere, odmAbove, odmOnNode, odmBelow);
  TOTANodePreservationMode = (onpNone, onpIntegrated, onpExternal);
  {$MINENUMSIZE 1}
  IOTAStructureContext = interface;
  IOTAStructureNode = interface;
  IOTAStructureNodeMenuItem = interface;

  IOTAStructureNotifier = interface(IOTANotifier)
    ['{F158EF17-342C-46AA-BA06-5DCD544354BF}']
    procedure StructureChanged(const Context: IOTAStructureContext);
    procedure NodeEdited(const Node: IOTAStructureNode);
    procedure NodeFocused(const Node: IOTAStructureNode);
    procedure NodeSelected(const Node: IOTAStructureNode);
    procedure DefaultNodeAction(const Node: IOTAStructureNode);
    procedure VisibleChanged(Visible: WordBool);
  end;

  // The base methods that are required by the StructureView form
  IOTABaseStructureView = interface(IDispatch)
    ['{04084727-3466-4220-9337-4F2A0CC17F87}']
    procedure BeginUpdate; safecall;
    procedure EndUpdate; safecall;
    function GetStructureContext: IOTAStructureContext; safecall;
    function GetStructureType: WideString; safecall;
    procedure SetStructureContext(const AContext: IOTAStructureContext); safecall; 
    procedure StructureChanged(Deferred: WordBool); safecall;
    procedure NodeSelected(const Node: IOTAStructureNode);
    procedure NodeFocused(const Node: IOTAStructureNode);
    procedure NodeEdited(const Node: IOTAStructureNode); 
  end;

  IOTAStructureView = interface(IOTABaseStructureView)
    ['{A37053F0-8E22-47A2-8E99-1CFFFC392BEF}']
    function AddNotifier(const Notifier: IOTAStructureNotifier): Integer; safecall;
    procedure ChildNodeUpdated(const ParentNode, ChildNode: IOTAStructureNode;
      AUpdateKind: TOTAChildUpdateKind); safecall;
    procedure ClearSelection; safecall;
    procedure FocusNode(const Node: IOTAStructureNode); safecall;
    function GetFirstSelected: IOTAStructureNode; safecall;
    function GetNextSelected(const ANode: IOTAStructureNode): IOTAStructureNode; safecall;
    function GetSelectedCount: Integer; safecall;
    function GetStructureType: WideString; safecall;
    procedure RemoveNotifier(Index: Integer); safecall;
    function RequestEdit(const Node: IOTAStructureNode; ForceEdit: WordBool): WordBool; safecall;
    procedure SetStructureContext(const AContext: IOTAStructureContext); safecall;
    procedure StructureChanged(Deferred: WordBool); safecall;
    procedure SelectNode(const Node: IOTAStructureNode); safecall;
    function AddBitmap(ABitmap: HBITMAP; StateImage: WordBool = False): Integer; safecall;
    function AddIcon(AIcon: HICON; StateImage: WordBool = False): Integer; safecall;
    function AddImageList(AImageList: HIMAGELIST; StateImage: WordBool = False): Integer; safecall;
    function ViewShowing: WordBool; safecall;
  end;

  IOTAStructureView110 = interface(IOTAStructureView)
    ['{72C311FE-4C8A-44AD-9F1F-9A846FE617F2}']
    function IsContextChanging: WordBool; safecall;
  end;

  IOTAStructureContext = interface(IDispatch)
    ['{67FD6512-C50F-4C83-8C2F-4E60340668D1}']
    function Get_ContextIdent: WideString; safecall;
    function Get_StructureType: WideString; safecall;
    function Get_ViewOptions: Integer; safecall;
    function Get_RootNodeCount: Integer; safecall;
    function GetRootStructureNode(Index: Integer): IOTAStructureNode; safecall;
    procedure NodeEdited(const Node: IOTAStructureNode); safecall;
    procedure NodeFocused(const Node: IOTAStructureNode); safecall;
    procedure NodeSelected(const Node: IOTAStructureNode); safecall;
    procedure DefaultNodeAction(const Node: IOTAStructureNode); safecall;
    function SameContext(const AContext: IOTAStructureContext): WordBool; safecall;
    procedure InitPopupMenu(const Node: IOTAStructureNode;
      const PopupMenu: IOTAStructureNodeMenuItem); safecall;
    procedure AddRootNode(const ANode: IOTAStructureNode; Index: Integer); safecall;
    procedure RemoveRootNode(const ANode: IOTAStructureNode); safecall;
    property ContextIdent: WideString read Get_ContextIdent;
    property StructureType: WideString read Get_StructureType;
    property ViewOptions: Integer read Get_ViewOptions;
    property RootNodeCount: Integer read Get_RootNodeCount;
  end;

  IOTAStructureContext110 = interface(IOTAStructureContext)
    ['{E60A76A6-8E44-4F67-85F3-06ED345BB2F4}']
    procedure ContextActivated; safecall;
  end;

  IOTAStructureNodeStatePreserver = interface(IDispatch)
    ['{F6189580-87EE-4985-8866-7712D8DB4872}']
    function Get_NodePreservationMode: TOTANodePreservationMode; safecall;
    procedure PreserveNodeStates; safecall;
    procedure RestoreNodeState(const Node: IOTAStructureNode); safecall;
    property NodePreservationMode: TOTANodePreservationMode read Get_NodePreservationMode;
  end;

  IOTAStructureUpdateSynchronizer = interface(IDispatch)
    ['{D53E0F35-B178-4541-8CCD-24196466855D}']
    procedure Lock; safecall;
    function TryLock: WordBool; safecall;
    procedure UnLock; safecall;
  end;

  INTAStructureContext = interface(IDispatch)
    ['{F413DD7C-532E-46EB-8238-C4FE5CDA9729}']
    function GetStructureControl: TWinControl; safecall;

    property StructureControl: TWinControl read GetStructureControl;
  end;

  IOTAStructureContextToolbar = interface(IDispatch)
    ['{55C848EB-2197-4B17-A9D0-696D3331B3CC}']
    function Get_ButtonCount: Integer; safecall;
    function GetButtonCaption(Index: Integer): WideString; safecall;
    function GetButtonEnabled(Index: Integer): WordBool; safecall;
    function GetButtonEnableDropDown(Index: Integer): WordBool; safecall;
    function GetButtonHasDropDown(Index: Integer): WordBool; safecall;
    function GetButtonHint(Index: Integer): WideString; safecall;
    function GetButtonImageIndex(Index: Integer): Integer; safecall;
    function GetButtonMenu(Index: Integer): IOTAStructureNodeMenuItem; safecall;
    function GetButtonSeparator(Index: Integer): WordBool; safecall;
    function GetButtonVisible(Index: Integer): WordBool; safecall;
    procedure Invoke(Index: Integer); safecall;
    property ButtonCount: Integer read Get_ButtonCount;
  end;

  IOTAStructureContextEditActions = interface(IDispatch)
    ['{EF0F0981-8E1B-468F-B063-4A5956EB4BDA}']
    function EditAction(Action: Integer): WordBool; safecall;
    function GetEditState: Integer; safecall;
  end;

  IOTAStructureContextKeyHandler = interface(IDispatch)
    ['{17A71EDB-A2FE-4358-ABD2-362011A8D547}']
    procedure KeyDown(const Node: IOTAStructureNode; KeyState: Integer; var KeyCode: Word); safecall;
    procedure KeyPressed(const Node: IOTAStructureNode; KeyState: Integer; var KeyChar: Word); safecall;
  end;

  IOTADragStructureContext = interface(IDispatch)
    ['{03DC0E9A-DED1-4E2A-BECE-328CB27D19B9}']
    function DragAllowed(const Node: IOTAStructureNode): WordBool; safecall;
    procedure DragDrop(const Node: IOTAStructureNode;
      DataObject: OleVariant; const FormatArray: WideString;
      X, Y: Integer; KeyState: Integer; Mode: TOTADropMode; var Effect: Integer); safecall;
    function DragOver(const Node: IOTAStructureNode;
      DataObject: OleVariant; State: TOTADragState;
      X, Y: Integer; KeyState: Integer; Mode: TOTADropMode; var Effect: Integer): WordBool; safecall;
    function GetDataObject: OleVariant; safecall;
  end;

  IOTAStructureNode = interface(IDispatch)
    ['{8A0802F5-C26C-4902-9D1F-8323F2F48F8C}']
    function AddChildNode(const ANode: IOTAStructureNode; Index: Integer = -1): Integer; safecall;
    function Get_Caption: WideString; safecall;
    function Get_ChildCount: Integer; safecall;
    function Get_Child(Index: Integer): IOTAStructureNode; safecall;
    function Get_Expanded: WordBool; safecall;
    procedure Set_Expanded(Value: WordBool); safecall;
    function Get_Focused: WordBool; safecall;
    procedure Set_Focused(Value: WordBool); safecall;
    function Get_Hint: WideString; safecall;
    function Get_ImageIndex: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Parent: IOTAStructureNode; safecall;
    function Get_Selected: WordBool; safecall;
    procedure Set_Selected(Value: WordBool); safecall;
    function Get_StateIndex: Integer; safecall;
    function Get_Data: Integer; safecall;
    procedure Set_Data(Value: Integer); safecall;
    procedure RemoveChildNode(Index: Integer); safecall;

    property Caption: WideString read Get_Caption;
    property ChildCount: Integer read Get_ChildCount;
    property Child[Index: Integer]: IOTAStructureNode read Get_Child;
    // The Data property is used by the structure view services to store certain
    // payload information.  DO NOT ATTEMPT TO USE THIS PROPERTY.  Implementers
    // of this interface should only store this value as purely opaque data.
    property Data: Integer read Get_Data write Set_Data;
    property Expanded: WordBool read Get_Expanded write Set_Expanded;
    property Focused: WordBool read Get_Focused write Set_Focused;
    property Hint: Widestring read Get_Hint;
    property ImageIndex: Integer read Get_ImageIndex;
    property Name: Widestring read Get_Name;
    property Parent: IOTAStructureNode read Get_Parent;
    property Selected: WordBool read Get_Selected write Set_Selected;
    property StateIndex: Integer read Get_StateIndex;
  end;

  IOTASortableStructureNode = interface(IDispatch)
    ['{24451294-D128-47E6-A8A5-00C3547C2F9C}']
    function Get_SortByIndex: WordBool; safecall;
    function Get_ItemIndex: Integer; safecall;

    property SortByIndex: WordBool read Get_SortByIndex;
    property ItemIndex: Integer read Get_ItemIndex;
  end;

  IOTAEditableStructureNode = interface(IDispatch)
    ['{B2CF6C45-4151-4B1A-B56B-B23534BB053E}']
    function Get_CanEdit: WordBool; safecall;
    function Get_EditCaption: WideString; safecall;
    procedure SetValue(const Value: WideString); safecall;

    property EditCaption: WideString read Get_EditCaption;
    property CanEdit: WordBool read Get_CanEdit;
  end;

  IOTANavigableStructureNode = interface(IDispatch)
  ['{81B1557A-E3F2-4E9C-AA69-CEBBA99D923F}']
    function Navigate: Boolean; safecall;
  end;

  IOTADragStructureNode = interface(IDispatch)
    ['{679BCA19-150F-4CC5-9023-CE24F5580F5B}']
    function DragAllowed: WordBool; safecall;
    procedure DragDrop(DataObject: OleVariant; const FormatArray: WideString;
      X, Y: Integer; KeyState: Integer; Mode: TOTADropMode; var Effect: Integer); safecall;
    function DragOver(DataObject: OleVariant; State: TOTADragState;
      X, Y: Integer; KeyState: Integer; Mode: TOTADropMode; var Effect: Integer): WordBool; safecall;
  end;

  IOTAStructureNodeMenuItem = interface(IDispatch)
    ['{7BF39892-F3D3-48F7-B5F3-664FDD438581}']
    procedure DeleteItem(Index: Integer); safecall;
    function Get_Caption: WideString; safecall;
    function Get_Checked: WordBool; safecall;
    function Get_Count: Integer; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_GroupIndex: Integer; safecall;
    function Get_ImageIndex: Integer; safecall;
    function Get_Item(Index: Integer): IOTAStructureNodeMenuItem; safecall;
    function Get_Name: WideString; safecall;
    function Get_RadioItem: WordBool; safecall;
    function Get_ShortCut: Integer; safecall;
    function Get_Visible: WordBool; safecall;
    procedure InsertItem(Index: Integer; const Item: IOTAStructureNodeMenuItem); safecall;
    procedure Invoke; safecall;

    property Caption: WideString read Get_Caption;
    property Checked: WordBool read Get_Checked;
    property Count: Integer read Get_Count;
    property Enabled: WordBool read Get_Enabled;
    property GroupIndex: Integer read Get_GroupIndex;
    property ImageIndex: Integer read Get_ImageIndex;
    property Item[Index: Integer]: IOTAStructureNodeMenuItem read Get_Item;
    property Name: WideString read Get_Name;
    property RadioItem: WordBool read Get_RadioItem;
    property ShortCut: Integer read Get_ShortCut;
    property Visible: WordBool read Get_Visible;
  end;

  IOTAStructureNodePopup = interface(IDispatch)
    ['{FAA6133B-F03F-4A93-9E86-C50EAD7348E6}']
    procedure InitPopupMenu(const PopupMenu: IOTAStructureNodeMenuItem); safecall;
  end;

  TStructureMenuItem = class(TMenuItem)
  private
    FStructureNodeMenuItem: IOTAStructureNodeMenuItem;
    procedure SetStructureNodeMenuItem(const AStructureNodeMenuItem: IOTAStructureNodeMenuItem);
  public
    constructor Clone(AMenuItem: TMenuItem; DestroyOriginal: Boolean = True);
    procedure Click; override;
    property StructureNodeMenuItem: IOTAStructureNodeMenuItem
      read FStructureNodeMenuItem write SetStructureNodeMenuItem;
  end;

  // !!!!Do NOT USE THIS INTERFACE EXECPT IN THIS UNIT!!!!
  IInternalStructureMenuItem = interface(IInterface)
    ['{7CC9A6ED-7C94-4CEF-B524-2BC3132468F4}']
    function GetMenuItem: TMenuItem;
  end;

  TStructureNodeMenuItem = class(TInterfacedObject, IDispatch, ISupportErrorInfo,
    IOTAStructureNodeMenuItem, IInternalStructureMenuItem)
  private
    FMenuItem: TMenuItem;
  protected
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; overload; stdcall;
    { ISupportErrorInfo }
    function InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
    { IOTAStructureNodeMenuItem }
    procedure DeleteItem(Index: Integer); safecall;
    function Get_Caption: WideString; safecall;
    function Get_Checked: WordBool; safecall;
    function Get_Count: Integer; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_GroupIndex: Integer; safecall;
    function Get_ImageIndex: Integer; safecall;
    function Get_Item(Index: Integer): IOTAStructureNodeMenuItem; safecall;
    function Get_Name: WideString; safecall;
    function Get_RadioItem: WordBool; safecall;
    function Get_ShortCut: Integer; safecall;
    function Get_Visible: WordBool; safecall;
    procedure InsertItem(Index: Integer; const Item: IOTAStructureNodeMenuItem); safecall;
    procedure Invoke; overload; safecall;
    { IInternalMenuItem }
    function GetMenuItem: TMenuItem;
  public
    constructor Create(AMenuItem: TMenuItem);
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

  { If an INTACustomEditorView also implements IOTACustomEditorViewStructure,
    then the IOTACustomEditorViewStructure will be queried for the context to
    use to populate the Structure view }
  IOTACustomEditorViewStructure = interface(IInterface)
    ['{8B75C91B-7B4A-4FBB-93F0-3C8A09585982}']
    function GetStructureContext: IOTAStructureContext;
    function GetStructureType: WideString;
  end;

  { If an INTACustomEditorSubView also implements IOTACustomEditorSubViewStructure,
    then the IOTACustomEditorSubViewStructure will be queried for the context to
    use to populate the Structure view }
  IOTACustomEditorSubViewStructure = interface(IInterface)
    ['{E597AA89-3E34-449A-BDFC-0B3731200150}']
    function GetStructureContext(const AContext: IInterface; AViewObject: TObject): IOTAStructureContext;
    function GetStructureType(const AContext: IInterface; AViewObject: TObject): WideString;
  end;

implementation

uses ComObj;

{ TStructureNodeMenuItem }

constructor TStructureNodeMenuItem.Create(AMenuItem: TMenuItem);
begin
  inherited Create;
  FMenuItem := AMenuItem;
  if FMenuItem is TStructureMenuItem then
    TStructureMenuItem(FMenuItem).FStructureNodeMenuItem := Self;
end;

procedure TStructureNodeMenuItem.DeleteItem(Index: Integer);
begin
  FMenuItem.Delete(Index);
end;

function TStructureNodeMenuItem.Get_Caption: WideString;
begin
  Result := FMenuItem.Caption;
end;

function TStructureNodeMenuItem.Get_Checked: WordBool;
begin
  Result := FMenuItem.Checked;
end;

function TStructureNodeMenuItem.Get_Count: Integer;
begin
  Result := FMenuItem.Count;
end;

function TStructureNodeMenuItem.Get_Enabled: WordBool;
begin
  Result := FMenuItem.Enabled;
end;

function TStructureNodeMenuItem.Get_ImageIndex: Integer;
begin
  Result := FMenuItem.ImageIndex;
end;

function TStructureNodeMenuItem.Get_Item(Index: Integer): IOTAStructureNodeMenuItem;
var
  MenuItem: TMenuItem;
begin
  MenuItem := FMenuItem.Items[Index];
  if not (MenuItem is TStructureMenuItem) then
    MenuItem := TStructureMenuItem.Clone(MenuItem);
  Result := TStructureMenuItem(MenuItem).StructureNodeMenuItem;
end;

function TStructureNodeMenuItem.Get_Name: WideString;
begin
  Result := FMenuItem.Name;
end;

function TStructureNodeMenuItem.Get_ShortCut: Integer;
begin
  Result := FMenuItem.ShortCut;
end;

function TStructureNodeMenuItem.Get_Visible: WordBool;
begin
  Result := FMenuItem.Visible;
end;

function TStructureNodeMenuItem.GetIDsOfNames(const IID: TGUID;
  Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TStructureNodeMenuItem.GetMenuItem: TMenuItem;
begin
  Result := FMenuItem;
end;

function TStructureNodeMenuItem.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TStructureNodeMenuItem.GetTypeInfoCount(
  out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TStructureNodeMenuItem.InsertItem(Index: Integer;
  const Item: IOTAStructureNodeMenuItem);
var
  MenuItem: TStructureMenuItem;
begin
  MenuItem := TStructureMenuItem.Create(FMenuItem.Owner);
  MenuItem.StructureNodeMenuItem := Item;
  FMenuItem.Insert(Index, MenuItem);
end;

function TStructureNodeMenuItem.InterfaceSupportsErrorInfo(
  const iid: TIID): HResult;
begin
  Result := S_OK;
end;

function TStructureNodeMenuItem.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TStructureNodeMenuItem.Invoke;
begin
  FMenuItem.Click;
end;

function TStructureNodeMenuItem.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IUnknown, '', '');
end;

function TStructureNodeMenuItem.Get_GroupIndex: Integer;
begin
  Result := FMenuItem.GroupIndex;
end;

function TStructureNodeMenuItem.Get_RadioItem: WordBool;
begin
  Result := FMenuItem.RadioItem;
end;

{ TStructureMenuItem }

procedure TStructureMenuItem.Click;
var
  InternalItem: IInternalStructureMenuItem;
begin
  if (FStructureNodeMenuItem = nil) or
    (Supports(FStructureNodeMenuItem, IInternalStructureMenuItem, InternalItem) and
    (InternalItem.GetMenuItem = Self)) then
    inherited Click
  else
    FStructureNodeMenuItem.Invoke;
end;

constructor TStructureMenuItem.Clone(AMenuItem: TMenuItem; DestroyOriginal: Boolean);
var
  I: Integer;
  LMenuItem: TMenuItem;
  LName: string;
begin
  inherited Create(AMenuItem.Owner);
  TStructureNodeMenuItem.Create(Self);
  if AMenuItem.Action <> nil then
    Action := AMenuItem.Action
  else
  begin
    Caption := AMenuItem.Caption;
    ShortCut := AMenuItem.ShortCut;
    Visible := AMenuItem.Visible;
    Enabled := AMenuItem.Enabled;
    Checked := AMenuItem.Checked;
    ImageIndex := AMenuItem.ImageIndex;
  end;
  for I := AMenuItem.Count - 1 downto 0 do
  begin
    LMenuItem := AMenuItem[I];
    AMenuItem.Delete(I);
    Insert(0, LMenuItem);
  end;
  if AMenuItem.Parent <> nil then
    AMenuItem.Parent.Insert(AMenuItem.Parent.IndexOf(AMenuItem), Self);
  if DestroyOriginal then
  begin
    LName := AMenuItem.Name;
    AMenuItem.Free;
    Name := LName;
  end;
end;

procedure TStructureMenuItem.SetStructureNodeMenuItem(const AStructureNodeMenuItem: IOTAStructureNodeMenuItem);
var
  I: Integer;
  MenuItem: TStructureMenuItem;
begin
  FStructureNodeMenuItem := AStructureNodeMenuItem;
  Caption := FStructureNodeMenuItem.Caption;
  Visible := FStructureNodeMenuItem.Visible;
  Enabled := FStructureNodeMenuItem.Enabled;
  ImageIndex := FStructureNodeMenuItem.ImageIndex;
  ShortCut := FStructureNodeMenuItem.ShortCut;
  Name := FStructureNodeMenuItem.Name;
  for I := 0 to FStructureNodeMenuItem.Count - 1 do
  begin
    MenuItem := TStructureMenuItem.Create(Owner);
    MenuItem.StructureNodeMenuItem := FStructureNodeMenuItem.Item[I];
    Add(MenuItem);
  end;
end;

end.
