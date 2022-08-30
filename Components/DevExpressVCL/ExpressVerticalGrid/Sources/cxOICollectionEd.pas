{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressVerticalGrid                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSVERTICALGRID AND ALL           }
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
unit cxOICollectionEd;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Menus, ExtCtrls, ComCtrls, ImgList, ActnList, ToolWin,
  dxCore, dxMessages, cxOI, cxClasses, dxForms;

type
  TcxColOption = (coAdd, coDelete, coMove);
  TcxColOptions = set of TcxColOption;

  { TcxCollectionEditor }

  TcxCollectionEditor = class(TdxForm, IcxRTTIInspectorHelper)
    acAdd: TAction;
    acDelete: TAction;
    acMoveDown: TAction;
    acMoveUp: TAction;
    acSelectAll: TAction;
    acTextLabels: TAction;
    acToolbar: TAction;
    ImageList: TImageList;
    ListView1: TListView;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    miMoveDown: TMenuItem;
    miMoveUp: TMenuItem;
    miTextLabels: TMenuItem;
    miToolbar: TMenuItem;
    N1: TMenuItem;
    Panel: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    Toolbar1: TToolbar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ActionList: TActionList;
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acSelectAllExecute(Sender: TObject);
    procedure acTextLabelsExecute(Sender: TObject);
    procedure acToolbarExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ListView1KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ListView1KeyPress(Sender: TObject; var Key: Char);
    procedure SelectAllCommandUpdate(Sender: TObject);
    procedure SelectionUpdate(Sender: TObject);
  private
    FClosing: Boolean;
    FCollectionPropertyName: string;
    FStateLock: Integer;
    FItemIDList: TList;
    FCollectionClassName: string;
    FSelectionError: Boolean;
    FColOptions: TcxColOptions;
    FInspector: TcxCustomRTTIInspector;
    function GetRegKey: string;
    procedure SetCollectionPropertyName(const Value: string);
    procedure AMDeferUpdate(var Msg: TMessage); message DXM_REFRESHCUSTOMIZATION;
    procedure SetColOptions(Value: TcxColOptions);
    procedure CloseEditor;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
  protected
    // IcxRTTIInspectorHelper
    procedure CloseNonModal(AInspector: TcxCustomRTTIInspector);
    procedure PropertyChanged(AInspector: TcxCustomRTTIInspector);
    //
    function  CanAdd(Index: Integer): Boolean; virtual;
    procedure Localize; virtual;
    procedure LockState;
    procedure UnlockState;
    property StateLock: Integer read FStateLock;
    procedure SelectAll(DoUpdate: Boolean = True);
    procedure SelectNone(DoUpdate: Boolean = True);
  public
    Collection: TCollection;
    Component: TComponent;
    property Options: TcxColOptions read FColOptions write SetColOptions;
    procedure ItemsModified(AInspector: TcxCustomRTTIInspector); virtual;
    function GetItemName(Index, ItemIndex: Integer): string;
    procedure GetSelection;
    procedure SetSelection;
    procedure UpdateListbox;
    property CollectionPropertyName: string read FCollectionPropertyName
      write SetCollectionPropertyName;
    property Inspector: TcxCustomRTTIInspector read FInspector;
  end;

  TcxCollectionEditorClass = class of TcxCollectionEditor;

  { TcxCollectionProperty }

  TcxCollectionProperty = class(TcxClassProperty)
  public
    destructor Destroy; override;
    procedure Edit; override;
    function GetAttributes: TcxPropertyAttributes; override;
    function GetEditorClass: TcxCollectionEditorClass; virtual;
    function GetColOptions: TcxColOptions; virtual;
    function IsDefaultValue: Boolean; override;
  end;

procedure cxShowCollectionEditor(AInspector: TcxCustomRTTIInspector; AComponent: TComponent;
  ACollection: TCollection; const PropertyName: string);
function cxShowCollectionEditorClass(AInspector: TcxCustomRTTIInspector;
  CollectionEditorClass: TcxCollectionEditorClass; AComponent: TComponent;
  ACollection: TCollection; const PropertyName: string;
  ColOptions: TcxColOptions = [coAdd, coDelete, coMove]): TcxCollectionEditor;

implementation

{$R *.dfm}

uses
  Types, Registry, TypInfo, cxVGridConsts;

type
  TCollectionAccess = class(TCollection); // used for protected method access
  TPersistentAccess = class(TPersistent);

var
  cxCollectionEditorsList: TList = nil;

function cxShowCollectionEditorClass(AInspector: TcxCustomRTTIInspector;
  CollectionEditorClass: TcxCollectionEditorClass; AComponent: TComponent;
  ACollection: TCollection; const PropertyName: string;
  ColOptions: TcxColOptions): TcxCollectionEditor;
var
  I: Integer;
begin
  if cxCollectionEditorsList = nil then
    cxCollectionEditorsList := TList.Create;
  for I := 0 to cxCollectionEditorsList.Count-1 do
  begin
    Result := TcxCollectionEditor(cxCollectionEditorsList[I]);
    with Result do
      if (Inspector = AInspector) and (Component = AComponent)
        and (Collection = ACollection)
        and (CompareText(CollectionPropertyName, PropertyName) = 0) then
      begin
        Show;
        BringToFront;
        Exit;
      end;
  end;
  Result := CollectionEditorClass.Create(Application);
  with Result do
  try
    Options := ColOptions;
    FInspector := AInspector;
    AInspector.AddListener(Result);
    Collection := ACollection;
    FCollectionClassName := ACollection.ClassName;
    Component := AComponent;
    CollectionPropertyName := PropertyName;
    UpdateListbox;
    Show;
  except
    Free;
  end;
end;

procedure cxShowCollectionEditor(AInspector: TcxCustomRTTIInspector; AComponent: TComponent;
  ACollection: TCollection; const PropertyName: string);
begin
  cxShowCollectionEditorClass(AInspector, TcxCollectionEditor, AComponent,
    ACollection, PropertyName);
end;

{ TcxCollectionProperty }

destructor TcxCollectionProperty.Destroy;
var
  I: Integer;
begin
  if Inspector.IsDestroying and (cxCollectionEditorsList <> nil) then
  begin
    for I := 0 to cxCollectionEditorsList.Count - 1 do
     with TcxCollectionEditor(cxCollectionEditorsList[I]) do
       if Inspector = Self.Inspector then CloseEditor;
  end;
  inherited Destroy;
end;

procedure TcxCollectionProperty.Edit;
var
  Obj: TPersistent;
begin
  Obj := GetComponent(0);
  while (Obj <> nil) and not (Obj is TComponent) do
    Obj := TPersistentAccess(Obj).GetOwner;
  cxShowCollectionEditorClass(Inspector, GetEditorClass,
    TComponent(Obj), TCollection(GetOrdValue), GetName, GetColOptions);
end;

function TcxCollectionProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaDialog, ipaReadOnly, ipaSubProperties];
end;

function TcxCollectionProperty.GetEditorClass: TcxCollectionEditorClass;
begin
  Result := TcxCollectionEditor;
end;

function TcxCollectionProperty.GetColOptions: TcxColOptions;
begin
  Result := [coAdd, coDelete, coMove];
end;

function TcxCollectionProperty.IsDefaultValue: Boolean;
begin
  Result := False;
end;

{ TcxCollectionEditor }

procedure TcxCollectionEditor.acAddExecute(Sender: TObject);
var
  Item: TListItem;
  PrevCount: Integer;
begin
  SelectNone(False);
  Collection.BeginUpdate;
  try
    PrevCount := Collection.Count + 1;
    Collection.Add;
    // Take into account collections that free items
    if PrevCount <> Collection.Count then
      UpdateListBox
    else
      ListView1.Selected := ListView1.Items.Add;
  finally
    Collection.EndUpdate;
  end;
  SetSelection;
  Item := ListView1.Items[ListView1.Items.Count-1];
  Item.Focused := True;
  Item.MakeVisible(False);
end;

procedure TcxCollectionEditor.acDeleteExecute(Sender: TObject);
var
  I, J: Integer;
begin
  Collection.BeginUpdate;
  try
    Inspector.InspectedObject := nil;
    if ListView1.Selected <> nil then
      J := ListView1.Selected.Index
    else
      J := -1;
    if ListView1.SelCount = Collection.Count then
      Collection.Clear
    else if ListView1.SelCount > 0 then
      for I := ListView1.Items.Count - 1 downto 0 do
        if ListView1.Items[I].Selected then
          Collection.Items[I].Free;
  finally
    Collection.EndUpdate;
  end;
  UpdateListbox;
  if J >= ListView1.Items.Count then
    J := ListView1.Items.Count - 1;
  if (J > -1) and (J < ListView1.Items.Count) then
    ListView1.Selected := ListView1.Items[J];
  SetSelection;
end;

procedure TcxCollectionEditor.acMoveDownExecute(Sender: TObject);
var
  I, InsPos: Integer;
begin
  if (ListView1.SelCount = 0) or
    (ListView1.SelCount = Collection.Count) then Exit;

  InsPos := ListView1.Items.Count - 1;
  while not ListView1.Items[InsPos].Selected do
    Dec(InsPos);
  if InsPos < (ListView1.Items.Count -1) then Inc(InsPos);

  Collection.BeginUpdate;
  try
     for I := ListView1.Items.Count - 1 downto 0 do
       if ListView1.Items[I].Selected then
       begin
         Collection.Items[I].Index := InsPos;
         Dec(InsPos);
       end;
  finally
    Collection.EndUpdate;
  end;
  GetSelection;
end;

procedure TcxCollectionEditor.acMoveUpExecute(Sender: TObject);
var
  I, InsPos: Integer;
begin
  if (ListView1.SelCount = 0) or
    (ListView1.SelCount = Collection.Count) then Exit;

  InsPos := 0;
  while not ListView1.Items[InsPos].Selected do
    Inc(InsPos);
  if InsPos > 0 then Dec(InsPos);

  Collection.BeginUpdate;
  try
     for I := 0 to ListView1.Items.Count - 1 do
       if ListView1.Items[I].Selected then
       begin
         Collection.Items[I].Index := InsPos;
         Inc(InsPos);
       end;
  finally
    Collection.EndUpdate;
  end;
  GetSelection;
end;

procedure TcxCollectionEditor.acSelectAllExecute(Sender: TObject);
begin
  SelectAll;
end;

procedure TcxCollectionEditor.acTextLabelsExecute(Sender: TObject);
begin
  with acTextLabels do
  begin
    Checked := not Checked;
    Toolbar1.ShowCaptions := Checked;
    if not Checked then
    begin
      Toolbar1.ButtonHeight := 24;
      Toolbar1.ButtonWidth := 24;
    end;
  end;
end;

procedure TcxCollectionEditor.acToolbarExecute(Sender: TObject);
begin
  with acToolbar do
  begin
    Checked := not Checked;
    Toolbar1.Visible := Checked;
  end;
end;

procedure TcxCollectionEditor.SetColOptions(Value: TcxColOptions);
begin
  FColOptions := Value;
  acAdd.Enabled := coAdd in Value;
end;

procedure TcxCollectionEditor.CloseEditor;
begin
  FClosing := True;
  Collection := nil;
  Component := nil;
  Close;
end;

procedure TcxCollectionEditor.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if (Message.Active <> WA_INACTIVE) and (Collection <> nil) then
    SetSelection;
end;

procedure TcxCollectionEditor.ItemsModified(AInspector: TcxCustomRTTIInspector);
begin
  if FClosing then exit;
  if Collection <> nil then
  begin
    UpdateListbox;
    GetSelection;
  end;
end;

function TcxCollectionEditor.GetItemName(Index, ItemIndex: Integer): string;
begin
  with TCollectionAccess(Collection) do
    if GetAttrCount < 1 then
      Result := Format('%d - %s',[ItemIndex, Collection.Items[ItemIndex].DisplayName])
    else Result := GetItemAttr(Index, ItemIndex);
end;

function TcxCollectionEditor.GetRegKey: string;
begin
  Result := '\Software\Borland\Delphi\3.0\Collection Editor';
end;

procedure TcxCollectionEditor.GetSelection;
var
  I: Integer;
  Item: TCollectionItem;
{$IFDEF RTTIMULTISELECTION}
  List: TList;
{$ENDIF}
begin
  LockState;
  try
    ListView1.Selected := nil;
  finally
    UnlockState;
  end;
{$IFDEF RTTIMULTISELECTION}
  List := TList.Create;
  try
    Inspector.GetSelections(List);
    if (List.Count = 0) or (List.Count > Collection.Count) then Exit;
    if not ((List[0] = Component) or (List[0] = Collection)
      or (TcxCollectionEditor(List[0]).GetOwner = Collection)) then Exit;
    if List.Count > ListView1.Items.Count then
      UpdateListbox;
  finally
    List.Free;
  end;
{$ELSE}
  UpdateListbox;
{$ENDIF}
  LockState;
  try
    for I := FItemIDList.Count - 1 downto 0 do
    begin
      Item := Collection.FindItemID(Integer(FItemIDList[I]));
      if Item <> nil then
        ListView1.Items[Item.Index].Selected := True
      else FItemIDList.Delete(I);
    end;
  finally
    UnlockState;
  end;
end;

procedure TcxCollectionEditor.LockState;
begin
  Inc(FStateLock);
end;

procedure TcxCollectionEditor.SetCollectionPropertyName(const Value: string);
begin
  if Value <> FCollectionPropertyName then
  begin
    FCollectionPropertyName := Value;
    Caption := Format(cxGetResourceString(@cxSvgRTTICollectionEditCaption),
      [Component.Name, DotSep, Value]);
  end;
end;

procedure TcxCollectionEditor.SetSelection;
var
  I: Integer;
{$IFDEF RTTIMULTISELECTION}
  List: TList;
{$ENDIF}
begin
  UpdateListBox;
  if FSelectionError then Exit;
  try
    if ListView1.SelCount > 0 then
    begin
    {$IFDEF RTTIMULTISELECTION}
      List := TList.Create;
      try
    {$ENDIF}
        FItemIDList.Clear;
        for I := 0 to ListView1.Items.Count - 1 do
          if ListView1.Items[I].Selected then
          begin
          {$IFDEF RTTIMULTISELECTION}
            List.Add(Collection.Items[I]);
          {$ENDIF}
            Inspector.InspectedObject := Collection.Items[I];
            FItemIDList.Add(Pointer(Collection.Items[I].ID));
          end;
    {$IFDEF RTTIMULTISELECTION}
        Inspector.SetSelections(List);
      finally
        List.Free;
      end;
    {$ENDIF}
    end
    else
      Inspector.InspectedObject := Collection;
    Inspector.RefreshInspectedProperties;
  except
    FSelectionError := True;
    Application.HandleException(ExceptObject);
    Close;
  end;
end;

procedure TcxCollectionEditor.UnlockState;
begin
  Dec(FStateLock);
end;

procedure TcxCollectionEditor.UpdateListbox;
var
  I, J: Integer;

  procedure UpdateSizes;
  var
    I: Integer;
  begin
    with TRegIniFile.Create(GetRegKey) do
    try
      Top := ReadInteger(FCollectionClassName, 'Top', 100);
      Left := ReadInteger(FCollectionClassName, 'Left', 100);
      Width := ReadInteger(FCollectionClassName, 'Width', Width);
      Height := ReadInteger(FCollectionClassName, 'Height', Height);
      ToolBar1.Visible := ReadBool(FCollectionClassName, 'Toolbar', True);
      acTextLabels.Checked := ReadBool(FCollectionClassName, 'TextLabels', False);
      ListView1.HandleNeeded;
      if ListView1.Columns.Count > 1 then
        for I := 0 to ListView1.Columns.Count - 1 do
          ListView1.Column[I].Width := ReadInteger(FCollectionClassName,
            Format('Column%d', [I]), ListView1.Column[I].WidthType);
    finally
      Free;
    end;
  end;

  procedure UpdateColumns;
  var
    I: Integer;
  begin
    if (Collection <> nil) and
      (((TCollectionAccess(Collection).GetAttrCount > 0) and
      (ListView1.Columns.Count <> TCollectionAccess(Collection).GetAttrCount)) or
      ((ListView1.Columns.Count = 0) and
      (TCollectionAccess(Collection).GetAttrCount < 1))) then
    begin
      ListView1.HandleNeeded;
      with TCollectionAccess(Collection) do
      begin
        if GetAttrCount >= 1 then
          for I := 0 to GetAttrCount - 1 do
            with ListView1.Columns.Add do
            begin
              Caption := GetAttr(I);
              Width := -2;
            end
        else
          with ListView1.Columns.Add do
            Width := -1;
        if GetAttrCount >= 1 then
          ListView1.ShowColumnHeaders := True
      end;
      UpdateSizes;
    end;
  end;

  procedure FetchItems(List: TStrings);
  var
    I, J: Integer;
    SubList: TStringList;
  begin
    if Collection <> nil then
      for I := 0 to Collection.Count - 1 do
        if CanAdd(I) then
        begin
          SubList := TStringList.Create;
          for J := 1 to TCollectionAccess(Collection).GetAttrCount - 1 do
            SubList.Add(GetItemName(J, I));
          List.AddObject(GetItemName(0, I), SubList);
        end;

  end;

  function ItemsEqual(ListItems: TListItems; Items: TStrings): Boolean;
  var
    I, J: Integer;
  begin
    Result := False;
    if ListItems.Count <> Items.Count then Exit;
    for I := 0 to ListItems.Count - 1 do
    begin
      if ListItems[I].Caption = Items[I] then
      begin
        for J := 0 to ListItems[I].SubItems.Count - 1 do
          if ListItems[I].SubItems[J] <> TStrings(Items.Objects[I])[J] then
            Exit;
      end
      else
        Exit;
    end;
    Result := True;
  end;

var
  TmpItems: TStringList;
begin
  if Collection = nil then Exit;
  LockState;
  try
    TmpItems := TStringList.Create;
    FetchItems(TmpItems);
    try
      if (TmpItems.Count = 0) or not ItemsEqual(ListView1.Items, TmpItems) then
      begin
        ListView1.Items.BeginUpdate;
        try
          UpdateColumns;
          ListView1.Items.Clear;
          for I := 0 to TmpItems.Count - 1 do
            with ListView1.Items.Add do
            begin
              Caption := TmpItems[I];
              for J := 0 to TStrings(TmpItems.Objects[I]).Count - 1 do
                SubItems.Add(TStrings(TmpItems.Objects[I])[J]);
            end;
        finally
          ListView1.Items.EndUpdate;
        end;
      end;
    finally
      for I := 0 to TmpItems.Count - 1 do
        TStrings(TmpItems.Objects[I]).Free;
      TmpItems.Free;
    end;
  finally
    UnlockState;
  end;
end;

procedure TcxCollectionEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  I: Integer;
begin
  if Component <> nil then
    Inspector.InspectedObject := Component;
  with TRegIniFile.Create(GetRegKey) do
  try
    EraseSection(FCollectionClassName);
    WriteInteger(FCollectionClassName, 'Left', Left);
    WriteInteger(FCollectionClassName, 'Top', Top);
    WriteInteger(FCollectionClassName, 'Width', Width);
    WriteInteger(FCollectionClassName, 'Height', Height);
    WriteBool(FCollectionClassName, 'TextLabels', acTextLabels.Checked);
    WriteBool(FCollectionClassName, 'Toolbar', ToolBar1.Visible);
    for I := 0 to ListView1.Columns.Count - 1 do
      WriteInteger(FCollectionClassName, Format('Column%d', [I]),
        ListView1.Column[I].WidthType);
  finally
    Free;
  end;
  Action := caFree;
  LockState;
end;

procedure TcxCollectionEditor.FormCreate(Sender: TObject);
begin
  Localize;
  FItemIdList := TList.Create;
  cxCollectionEditorsList.Add(Self);
end;

procedure TcxCollectionEditor.FormDestroy(Sender: TObject);
begin
  Inspector.RemoveListener(Self);
  FItemIdList.Free;
  if cxCollectionEditorsList <> nil then
    cxCollectionEditorsList.Remove(Self);
end;

procedure TcxCollectionEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_PROCESSKEY then
    with Inspector do
      if CanFocusEx then
      begin
        SetFocus;
        ShowEdit;
      end;
end;

procedure TcxCollectionEditor.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    with Inspector do
      if CanFocusEx then
      begin
        SetFocus;
        ShowEdit;
      end;
end;

procedure TcxCollectionEditor.FormShow(Sender: TObject);
begin
  MakeFullyVisible;
end;

procedure TcxCollectionEditor.ListView1Change(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  Msg: TMsg;
begin
  if (Change = ctState) and (FStateLock = 0) then
    if not PeekMessage(Msg, Handle, DXM_REFRESHCUSTOMIZATION, DXM_REFRESHCUSTOMIZATION, PM_NOREMOVE) then
      PostMessage(Handle, DXM_REFRESHCUSTOMIZATION, 0, 0);
end;

procedure TcxCollectionEditor.ListView1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Item: TListItem;
  I, J, InsPos: Integer;
  L: TList;
begin
  Item := ListView1.GetItemAt(X, Y);
  if Item <> nil then
    InsPos := Item.Index
  else Exit;
  L := TList.Create;
  try
    for I := 0 to ListView1.Items.Count - 1 do
      if ListView1.Items[I].Selected then
        L.Add(Collection.Items[I]);

    Collection.BeginUpdate;
    try
      for I := 0 to L.Count - 1 do
      with TCollectionItem(L[I]) do
      begin
        J := Index;
        Index := InsPos;
        if (J > InsPos) and (InsPos < Collection.Count) then
          Inc(InsPos);
      end;
    finally
      Collection.EndUpdate;
    end;
  finally
    L.Free;
  end;
  GetSelection;
end;

procedure TcxCollectionEditor.ListView1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Item: TListItem;
begin
  Item := ListView1.GetItemAt(X, Y);
  Accept := (Item <> nil) and (Source = ListView1) and
    (not Item.Selected);
end;

procedure TcxCollectionEditor.ListView1KeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_PROCESSKEY then
    Inspector.SetFocus;
end;

procedure TcxCollectionEditor.ListView1KeyPress(Sender: TObject;
  var Key: Char);
begin
  if dxCharInSet(Key, ['!'..'~']) and Inspector.CanFocusEx then
  begin
    Inspector.SetFocus;
    Inspector.ShowEditByKey(Key);
    Key := #0;
  end;
end;

procedure TcxCollectionEditor.SelectAllCommandUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListView1.Items.Count > 0;
end;

procedure TcxCollectionEditor.AMDeferUpdate(var Msg: TMessage);
begin
  if FStateLock = 0 then
  begin
    if Msg.WParam = 0 then
      SetSelection
    else
      ItemsModified(nil);
  end
  else
    PostMessage(Handle, DXM_REFRESHCUSTOMIZATION, Msg.WParam, Msg.LParam);
end;

procedure TcxCollectionEditor.SelectionUpdate(Sender: TObject);
var
  Enabled: Boolean;
begin
  Enabled := ListView1.Selected <> nil;
  if Enabled then
    if Sender = acDelete then
      Enabled := coDelete in Options
    else
      if (Sender = acMoveUp) or (Sender = acMoveDown) then
        Enabled := coMove in Options;
  (Sender as TAction).Enabled := Enabled;
end;

procedure TcxCollectionEditor.SelectAll(DoUpdate: Boolean);
var
  I: Integer;
begin
  LockState;
  ListView1.Items.BeginUpdate;
  try
    for I := 0 to Listview1.Items.Count-1 do
      Listview1.Items[I].Selected := True;
  finally
    ListView1.Items.EndUpdate;
    UnlockState;
    if DoUpdate then SetSelection;
  end;
end;

procedure TcxCollectionEditor.SelectNone(DoUpdate: Boolean);
var
  I: Integer;
begin
  LockState;
  ListView1.Items.BeginUpdate;
  try
    for I := 0 to Listview1.Items.Count-1 do
      Listview1.Items[I].Selected := False;
  finally
    ListView1.Items.EndUpdate;
    UnlockState;
    if DoUpdate then SetSelection;
  end;
end;

procedure TcxCollectionEditor.CloseNonModal(AInspector: TcxCustomRTTIInspector);
begin
  CloseEditor;
end;

procedure TcxCollectionEditor.PropertyChanged(
  AInspector: TcxCustomRTTIInspector);
begin
  if FStateLock > 0 then Exit;
    ItemsModified(AInspector);
end;

function TcxCollectionEditor.CanAdd(Index: Integer): Boolean;
begin
  Result := True;
end;

procedure TcxCollectionEditor.Localize;
begin
  acAdd.Caption := cxGetResourceString(@cxSvgRTTICollectionAdd);
  acAdd.Hint := cxGetResourceString(@cxSvgRTTICollectionAddHint);
  acDelete.Caption := cxGetResourceString(@cxSvgRTTICollectionDelete);
  acDelete.Hint := cxGetResourceString(@cxSvgRTTICollectionDeleteHint);
  acSelectAll.Caption := cxGetResourceString(@cxSvgRTTICollectionSelectAll);
  acToolbar.Caption := cxGetResourceString(@cxSvgRTTICollectionToolbar);
  acTextLabels.Caption := cxGetResourceString(@cxSvgRTTICollectionTextLabel);
  acMoveUp.Caption := cxGetResourceString(@cxSvgRTTICollectionMoveUp);
  acMoveUp.Hint := cxGetResourceString(@cxSvgRTTICollectionMoveUpHint);
  acMoveDown.Caption := cxGetResourceString(@cxSvgRTTICollectionMoveDown);
  acMoveDown.Hint := cxGetResourceString(@cxSvgRTTICollectionMoveDownHint);
end;

initialization
  cxRegisterPropertyEditor(TypeInfo(TCollection), nil, '', TcxCollectionProperty);

finalization
  cxCollectionEditorsList.Free;
  cxCollectionEditorsList := nil;
end.
