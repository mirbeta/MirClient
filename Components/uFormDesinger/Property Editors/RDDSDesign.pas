{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

{*******************************************************}
{                  Dataset Designer                     }
{*******************************************************}

unit RDDSDesign;

interface

{$IFDEF MSWINDOWS}
uses Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, DB, DBCtrls, DesignIntf, DesignEditors,
  DesignWindows, Menus, ActnPopup, (*IDEWideStdCtrls,*)
  WideStrings, PlatformDefaultStyleActnCtrls;
{$ENDIF}

{$IFDEF LINUX}
uses Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, DB, DBCtrls, DesignIntf, DesignEditors,
  DsnDBCst, DesignWindows, Menus {, DrpCtrls}, Libc;
{$ENDIF}

type

  TSelectionProc = function(Field: TField): Boolean of object;

  TDSDesigner = class;
  TDSDesignerClass = class of TDSDesigner;

  TFieldsEditor = class(TDesignWindow)
    Panel1: TPanel;
    DataSource: TDataSource;
    LocalMenu: TPopupActionBar;
    AddItem: TMenuItem;
    NewItem: TMenuItem;
    N1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    DeleteItem: TMenuItem;
    SelectAllItem: TMenuItem;
    FieldListBox: TListBox;
    DBNavigator: TDBNavigator;
    Addallfields1: TMenuItem;
    AggListBox: TListBox;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddItemClick(Sender: TObject);
    procedure DeleteItemClick(Sender: TObject);
    procedure FieldListBoxDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FieldListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure AListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure NewItemClick(Sender: TObject);
    procedure SelectTable(Sender: TObject);
    procedure AListBoxClick(Sender: TObject);
    procedure AListBoxKeyPress(Sender: TObject; var Key: Char);
    procedure ClearAllClick(Sender: TObject);
    procedure FieldListBoxStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure SelectAllItemClick(Sender: TObject);
    procedure CutItemClick(Sender: TObject);
    procedure CopyItemClick(Sender: TObject);
    procedure PasteItemClick(Sender: TObject);
    procedure LocalMenuPopup(Sender: TObject);
    procedure AddAllFields(Sender: TObject);
  private
    FDSDesignerClass: TDSDesignerClass;
    FDragObject: TDragObject;
    FDSDesigner: TDSDesigner;
    {FForm: TCustomForm;}
    FDataset: TDataset;
    FFocusRectItem: Integer;
    FMinWidth, FMinHeight: Integer;
    FFieldListUpdated: Boolean;
    FUpdateCount: Integer;
    procedure AddFields(All: Boolean);
    procedure BeginUpdate;
    procedure Copy;
    function CreateFields(FieldsList: TListBox): TField;
    procedure Cut;
    procedure Endupdate;
    procedure MoveFields(MoveOffset: Integer);
    procedure Paste;
    procedure RemoveFields(Listbox: TListbox);
    procedure SelectAll;
    procedure RestoreSelection(List: TListBox; var Selection: TWideStringList;
      ItemIndex, TopIndex: Integer; RestoreUpdate: Boolean);
    procedure SaveSelection(List: TListBox; var Selection: TWideStringList;
      var ItemIndex, TopIndex: Integer; NoUpdate: Boolean);
    procedure SetDataset(Value: TDataset);
    procedure UpdateDisplay;
    procedure UpdateCaption;
    procedure UpdateFieldList;
    procedure UpdateSelection;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    function GetActiveListbox: TListbox;
  protected
    procedure Activated; override;
    procedure CheckFieldDelete;
    procedure CheckFieldAdd;
    function UniqueName(Component: TComponent): string; override;
  public
    destructor Destroy; override;
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent); override;
    function GetEditState: TEditState; override;
    function EditAction(Action: TEditAction): Boolean; override;
    function ForEachSelection(Proc: TSelectionProc): Boolean;
    procedure ItemsModified(const Designer: IDesigner); override;
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections); override;
    function DoNewField: TField;
    function DoNewLookupField(const ADataSet, AKey, ALookup, AResult,
      AType: string; ASize: Word): TField;
    function DoAddFields(All: Boolean): TField;
    {property Form: TCustomForm read FForm write FForm;}
    property Dataset: TDataset read FDataset write SetDataset;
    property DSDesignerClass: TDSDesignerClass read FDSDesignerClass write FDSDesignerClass;
    property DSDesigner: TDSDesigner read FDSDesigner;
  end;

{ TDSDesigner }

  TDSDesigner = class(TDatasetDesigner)
  private
    FFieldsEditor: TFieldsEditor;
  public
    destructor Destroy; override;
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;

    procedure BeginCreateFields; virtual;
    procedure BeginUpdateFieldDefs; virtual;
    function DoCreateField(const FieldName: WideString; Origin: string): TField; virtual;
    procedure EndCreateFields; virtual;
    procedure EndUpdateFieldDefs; virtual;
    function GetControlClass(Field: TField): string; virtual;
    procedure InitializeMenu(Menu: TPopupMenu); virtual;
    function SupportsAggregates: Boolean; virtual;
    function SupportsInternalCalc: Boolean; virtual;
    procedure UpdateMenus(Menu: TPopupMenu; EditState: TEditState); virtual;
    property FieldsEditor: TFieldsEditor read FFieldsEditor;
  end;

procedure ShowFieldsEditor(Designer: IDesigner; ADataset: TDataset;
  DesignerClass: TDSDesignerClass);
function CreateFieldsEditor(Designer: IDesigner; ADataset: TDataset;
  DesignerClass: TDSDesignerClass; var Shared: Boolean): TFieldsEditor;

function CreateUniqueName(Dataset: TDataset; const FieldName: string;
  FieldClass: TFieldClass; Component: TComponent): string;

var
  DesignerCount: Integer;

implementation

uses
  Dialogs, TypInfo, Math, LibHelp, RDDSAdd, RDDSDefine, DesignConst, ToolsAPI, StructureViewAPI, Character;

{ TDSDesigner }

destructor TDSDesigner.Destroy;
begin
  if FFieldsEditor <> nil then
  begin
    FFieldsEditor.FDSDesigner := nil;
    FFieldsEditor.Release;
  end;
  inherited Destroy;
end;

procedure TDSDesigner.DataEvent(Event: TDataEvent; Info: Longint);
begin
  if Event = deFieldListChange then FFieldsEditor.UpdateFieldList;
end;

function TDSDesigner.GetControlClass(Field: TField): string;
begin
  Result := '';
end;

function TDSDesigner.SupportsAggregates: Boolean;
begin
  Result := False;
end;

function TDSDesigner.SupportsInternalCalc: Boolean;
begin
  Result := False;
end;

procedure TDSDesigner.BeginUpdateFieldDefs;
begin
end;

procedure TDSDesigner.EndUpdateFieldDefs;
begin
end;

procedure TDSDesigner.BeginCreateFields;
var
  StructureView: IOTAStructureView;
begin
  if Supports(BorlandIDEServices, IOTAStructureView, StructureView) then
    StructureView.BeginUpdate;
end;

procedure TDSDesigner.EndCreateFields;
var
  StructureView: IOTAStructureView;
begin
  if Supports(BorlandIDEServices, IOTAStructureView, StructureView) then
    StructureView.EndUpdate;
end;

procedure TDSDesigner.InitializeMenu(Menu: TPopupMenu);
begin
end;

procedure TDSDesigner.UpdateMenus(Menu: TPopupMenu; EditState: TEditState);
begin
end;

function TDSDesigner.DoCreateField(const FieldName: WideString; Origin: string): TField;
var
  FieldDef: TFieldDef;
  ParentField: TField;
  SubScript,
  ShortName,
  ParentFullName: String;
begin
  FieldDef := Dataset.FieldDefList.FieldByName(FieldName);
  ParentField := nil;
  if Dataset.ObjectView then
  begin
    if FieldDef.ParentDef <> nil then
    begin
      if FieldDef.ParentDef.DataType = ftArray then
      begin
        { Strip off the subscript to determine the parent's full name }
        SubScript := Copy(FieldName, Pos('[', FieldName), MaxInt);
        ParentFullName := Copy(FieldName, 1, Length(FieldName) - Length(SubScript));
        ShortName := FieldDef.ParentDef.Name + SubScript;
      end
      else
      begin
        if faUnNamed in FieldDef.ParentDef.Attributes then
          ParentFullName := FieldDef.ParentDef.Name else
          ParentFullName := ChangeFileExt(FieldName, '');
        ShortName := FieldDef.Name;
      end;
      ParentField := Dataset.FieldList.Find(ParentFullName);
      if ParentField = nil then
        ParentField := DoCreateField(ParentFullName, Origin);
    end
    else
      ShortName := FieldDef.Name;
  end
  else
    ShortName := FieldName;
  Result := FieldDef.CreateField(DataSet.Owner, ParentField as TObjectField, ShortName, False);
  try
    Result.Origin := Origin;
    Result.Name := CreateUniqueName(Dataset, FieldName, TFieldClass(Result.ClassType), nil);
  except
    Result.Free;
    raise;
  end;
end;

{ Utility functions }

procedure ShowFieldsEditor(Designer: IDesigner; ADataset: TDataset;
  DesignerClass: TDSDesignerClass);
var
  FieldsEditor: TFieldsEditor;
  vShared: Boolean;
begin
  FieldsEditor := CreateFieldsEditor(Designer, ADataSet, DesignerClass, vShared);
  if FieldsEditor <> nil then
    FieldsEditor.Show;
end;

function CreateFieldsEditor(Designer: IDesigner; ADataset: TDataset;
  DesignerClass: TDSDesignerClass; var Shared: Boolean): TFieldsEditor;
begin
  Shared := True;
  if ADataset.Designer <> nil then
  begin
    Result := (ADataset.Designer as TDSDesigner).FFieldsEditor;
  end
  else
  begin
    Result := TFieldsEditor.Create(Application);
    Result.DSDesignerClass := DesignerClass;
    Result.Designer := Designer;
    {Result.Form := Designer.Form;}
    Result.Dataset := ADataset;
    Shared := False;
  end;
end;

function GenerateName(Dataset: TDataset; FieldName: string;
  FieldClass: TFieldClass; Number: Integer): string;
var
  Fmt: string;

  function Alpha(C: Char): Boolean; inline;
  begin
    Result := TCharacter.IsLetter(C) or (C = '_');
  end;

  function AlphaNumeric(C: Char): Boolean; inline;
  begin
    Result := TCharacter.IsLetterOrDigit(C) or (C = '_');
  end;

  procedure CrunchFieldName;
  var
    I: Integer;
  begin
    I := 1;
    while I <= Length(FieldName) do
    begin
      if AlphaNumeric(FieldName[I]) then
        Inc(I)
      else
        Delete(FieldName, I, 1);
    end;
  end;

begin
  CrunchFieldName;
  if (FieldName = '') or not Alpha(FieldName[1]) then
  begin
    if FieldClass <> nil then
      FieldName := FieldClass.ClassName + FieldName else
      FieldName := 'Field' + FieldName;
    if FieldName[1] = 'T' then Delete(FieldName, 1, 1);
    CrunchFieldName;
  end;
  Fmt := '%s%s%d';
  if Number < 2 then Fmt := '%s%s';
  Result := Format(Fmt, [Dataset.Name, FieldName, Number]);
end;

function CreateUniqueName(Dataset: TDataset; const FieldName: string;
  FieldClass: TFieldClass; Component: TComponent): string;
var
  I: Integer;

  function IsUnique(const AName: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    with Dataset.Owner do
      for I := 0 to ComponentCount - 1 do
        if (Component <> Components[i]) and (CompareText(AName, Components[I].Name) = 0) then Exit;
    Result := True;
  end;

begin
  for I := 1 to MaxInt do
  begin
    Result := GenerateName(Dataset, FieldName, FieldClass, I);
    if IsUnique(Result) then Exit;
  end;
end;

{ TDragFields }

type
  TDragFields = class(TDragControlObject)
  private
    FEditor: TFieldsEditor;
  public
    constructor Create(AControl: TControl; AEditor: TFieldsEditor); reintroduce;
    property Editor: TFieldsEditor read FEditor;
  end;

constructor TDragFields.Create(AControl: TControl; AEditor: TFieldsEditor);
begin
  inherited Create(AControl);
  FEditor := AEditor;
end;

{$IFDEF MSWINDOWS}

{ TFieldsTarget }

type
  TFieldsTarget = class(TDragTarget)
  public
    function DragOver(Target, Source: TObject; X, Y: Integer;
      State: TDragState): Boolean; override;
    procedure DragDrop(Target, Source: TObject; X, Y: Integer); override;
  end;

function TFieldsTarget.DragOver(Target, Source: TObject; X, Y: Integer;
  State: TDragState): Boolean;
begin
  Result := Designer.Root is TWinControl;
end;

procedure TFieldsTarget.DragDrop(Target, Source: TObject; X, Y: Integer);
var
  SourceRoot: TComponent;
  Control: TControl;
  I: Integer;
  Editor: TFieldsEditor;
  FieldList: TList;
  Field: TField;
begin
  SourceRoot := TDragFields(Source).Editor.Designer.GetRoot;
  if not Designer.IsComponentLinkable(SourceRoot) then
    if MessageDlg(Format('SDSLinkForms%s%s', [Designer.GetRoot.Name,
      SourceRoot.Name]), mtConfirmation, mbYesNoCancel, 0) <> idYes then
        Exit
    else
      Designer.MakeComponentLinkable(SourceRoot);
  FieldList := TList.Create;
  try
    { Collect the fields before creating the controls since creating the first
      control will remove all the sections }
    Editor := TDragFields(Source).Editor;
    with Editor do
    begin
      for I := 0 to FieldListBox.Items.Count - 1 do
        if FieldListBox.Selected[I] then
          FieldList.Add(FieldListBox.Items.Objects[I]{Dataset.FieldByName(FieldListBox.Items[I])});
    end;
    Screen.Cursor := crHourGlass;
    try
      for I := 0 to FieldList.Count - 1 do
      begin
        Field := TField(FieldList[I]);
        Control := CreateFieldControl(Designer, Field,
          Editor.DSDesigner.GetControlClass(Field), TComponent(Target), X, Y, True);
        Y := Control.Top + Control.Height + 5;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  finally
    FieldList.Free;
  end;
end;

{$R *.dfm}
{$ENDIF}

{ TFieldsEditor }

destructor TFieldsEditor.Destroy;
begin
  FDragObject.Free;
  FDragObject := nil;
  inherited Destroy;
end;

procedure TFieldsEditor.UpdateDisplay;
begin
  UpdateFieldList;
  UpdateCaption;
  UpdateSelection;
end;

procedure TFieldsEditor.SaveSelection(List: TListBox;
  var Selection: TWideStringList; var ItemIndex, TopIndex: Integer;
  NoUpdate: Boolean);
var
  I: Integer;
begin
  Selection := TWideStringList.Create;
  try
    ItemIndex := List.ItemIndex;
    TopIndex := List.TopIndex;
    with List do
      for I := 0 to Items.Count - 1 do
        if Selected[I] then Selection.Add(Items[I]);
    if NoUpdate then List.Items.BeginUpdate;
  except
    Selection.Free;
    Selection := nil;
  end;
end;

procedure TFieldsEditor.RestoreSelection(List: TListBox;
  var Selection: TWideStringList; ItemIndex, TopIndex: Integer;
  RestoreUpdate: Boolean);
var
  I: Integer;
begin
  try
    with List do
      for I := 0 to Items.Count - 1 do
        Selected[I] := Selection.IndexOf(Items[I]) <> -1;
    if TopIndex <> -1 then List.TopIndex := TopIndex;
    if ItemIndex <> -1 then List.ItemIndex := ItemIndex;
  finally
    if RestoreUpdate then List.Items.EndUpdate;
    List.Invalidate;
    Selection.Free;
    Selection := nil;
    UpdateSelection;
  end;
end;

procedure TFieldsEditor.UpdateCaption;
var
  NewCaption: string;
begin
  if (DataSet = nil) or (csDestroying in Dataset.ComponentState) then
    Exit;
  if Dataset.Owner <> nil then
    NewCaption := Format(SDatasetEditor, [Dataset.Owner.Name, DotSep,
      Dataset.Name]);
  if Caption <> NewCaption then Caption := NewCaption;
end;

procedure TFieldsEditor.UpdateFieldList;
var
  ItemIndex, TopIndex: Integer;
  Selection: TWideStringList;
  EnableList: Boolean;
  I: Integer;
  Field: TField;
  FieldName: string;
  ActiveListbox: TListbox;
begin
  if FUpdateCount = 0 then
  begin
    FFieldListUpdated := False;
    ActiveListbox := GetActiveListbox;
    SaveSelection(ActiveListBox, Selection, ItemIndex, TopIndex, True);
    try
      FieldListBox.Clear;
      AggListBox.Clear;
      EnableList := False;
      try
        if Dataset = nil then Exit;
        for I := 0 to Dataset.FieldList.Count - 1 do
        begin
          Field := Dataset.FieldList[I];
          if not (csDestroying in Field.ComponentState) and
             (Field.Owner = Dataset.Owner) then
          begin
            FieldName := Field.FullName;
            if FieldName = '' then
              FieldName := Format('<%s>', [Dataset.FieldList[I].Name]);
            FieldListbox.Items.AddObject(FieldName, Field);
          end;
        end;

        for I := 0 to Dataset.AggFields.Count - 1 do
        begin
          Field := Dataset.AggFields[I];
          if not (csDestroying in Field.ComponentState) and
             (Field.Owner = Dataset.Owner) then
          begin
            FieldName := Field.FullName;
            if FieldName = '' then
              FieldName := Format('<%s>', [Dataset.AggFields[I].Name]);
            AggListbox.Items.AddObject(FieldName, Field);
          end;
        end;
        with AggListbox do
          if Items.Count > 0 then
          begin
            Visible := True;
            Splitter1.Visible := True;
          end
          else
          begin
            Visible := False;
            Splitter1.Visible := False;
          end;

        EnableList := True;
      finally
        FieldListBox.Enabled := EnableList;
        AggListBox.Enabled := EnableList and (AggListBox.Items.Count > 0);
      end;
    finally
      if ActiveListBox.Visible then
        RestoreSelection(ActiveListBox, Selection, ItemIndex, TopIndex, True)
      else if ActiveListBox = AggListbox then
        ActiveListBox.Items.EndUpdate;
    end;
  end else
    FFieldListUpdated := True;
end;

procedure TFieldsEditor.UpdateSelection;
var
  I: Integer;
  Field: TField;
  ComponentList: IDesignerSelections;
begin
  if Active then
  begin
    ComponentList := TDesignerSelections.Create;
    try
      with GetActiveListBox do
        for I := 0 to Items.Count - 1 do
          if Selected[I] then
          begin
            Field := TField(Items.Objects[I]){Dataset.FindField(Items[I])};
            if Field <> nil then ComponentList.Add(Field);
          end;
      if ComponentList.Count = 0 then ComponentList.Add(Dataset);
    except
      raise;
    end;
    Designer.SetSelections(ComponentList);
  end;
end;

function TFieldsEditor.CreateFields(FieldsList: TListBox): TField;
var
  I: Integer;
  ItemIndex, TopIndex: Integer;
  Selection: TWideStringList;
  FocusedListbox: TListbox;
  Fields: TWideStringList;
begin
  Result := nil;
  FocusedListbox := nil;
  if Visible then
  begin
    FocusedListBox := GetActiveListBox;
    SaveSelection(FocusedListbox, Selection, ItemIndex, TopIndex, False);
  end;
  try
    Screen.Cursor := crHourGlass;
    try
      BeginUpdate;
      try
        FDSDesigner.BeginDesign;
        try
          Fields := TWideStringList.Create;
          try
            for i := 0 to FieldsList.Items.Count - 1 do
              if FieldsList.Selected[i] then
                Fields.Add(FieldsList.Items[i]);
            DSDesigner.BeginCreateFields;
            try
              for I := 0 to Fields.Count - 1 do
                Result := DSDesigner.DoCreateField(Fields[I], '');
            finally
              DSDesigner.EndCreateFields;
            end;
          finally
            Fields.Free;
          end;
        finally
          FDSDesigner.EndDesign;
        end;
      finally
        EndUpdate;
        Designer.Modified;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  finally
    if FocusedListbox <> nil then
    begin
      UpdateDisplay;
      RestoreSelection(FocusedListBox, Selection, -1, -1, False);
    end;
  end;
end;

procedure TFieldsEditor.SelectAll;
var
  I: Integer;
begin
  with FieldListBox do
    for I := 0 to Items.Count - 1 do Selected[I] := True;
end;

procedure TFieldsEditor.RemoveFields(Listbox: TListbox);
var
  I, Focused: Integer;
begin
  CheckFieldDelete;
  try
    BeginUpdate;
    try
      FDSDesigner.BeginDesign;
      try
        Focused := ListBox.ItemIndex;
        with ListBox do
          for I := Items.Count - 1 downto 0 do
            if Selected[I] then
              TField(Items.Objects[I]).Free;
              //Dataset.FindField(Items[I]).Free;
      finally
        FDSDesigner.EndDesign;
      end;
    finally
      EndUpdate;
      Designer.Modified;
    end;
  finally
    UpdateDisplay;
  end;
  if Focused <> -1 then
  begin
    Focused := Min(Focused, ListBox.Items.Count - 1);
    ListBox.ItemIndex := Focused;
    ListBox.Selected[Focused] := True;
    UpdateSelection;
  end;
  if (ListBox = AggListBox) and (ListBox.Items.Count = 0) then
    FieldListBox.SetFocus
  else
    ListBox.SetFocus;
end;

procedure TFieldsEditor.MoveFields(MoveOffset: Integer);
var
  I, E: Integer;
begin
  try
    DataSet.DisableControls;
    try
      with FieldListBox do
      begin
        I := 0;
        E := Items.Count;
        if MoveOffset > 0 then
        begin
          I := E - 1;
          E := -1;
        end;
        while I <> E do
        begin
          if Selected[I] then
            with TField(Items.Objects[I]){Dataset.FieldByName(Items[I])} do
              Index := Index + MoveOffset;
          Inc(I, -MoveOffset);
        end;
      end;
    finally
      DataSet.EnableControls;
    end;
  finally
    UpdateDisplay;
    Designer.Modified;
  end;
end;

procedure TFieldsEditor.SetDataset(Value: TDataset);
begin
  if FDataSet <> Value then
  begin
    if FDataSet <> nil then
    begin
      FreeAndNil(FDSDesigner);
      DataSource.DataSet := nil;
    end;
    FDataset := Value;
    if FDataSet <> nil then
    begin
      FDSDesigner := DSDesignerClass.Create(Value);
      FDSDesigner.FFieldsEditor := Self;
      FDSDesigner.InitializeMenu(LocalMenu);
      DataSource.DataSet := Value;
      UpdateDisplay;
    end
    else
      Release;
  end;
end;

procedure TFieldsEditor.FormCreate(Sender: TObject);
begin
  Inc(DesignerCount);
  FMinWidth := Width;
  FMinHeight := Height;
  HelpContext := hcDataSetDesigner;
end;

procedure TFieldsEditor.FormDestroy(Sender: TObject);
begin
  if FDSDesigner <> nil then
  begin
    { Destroy the designer if the editor is destroyed }
    FDSDesigner.FFieldsEditor := nil;
    FDSDesigner.Free;
  end;
  Dec(DesignerCount);
end;

procedure TFieldsEditor.AddFields(All: Boolean);
begin
  DoAddFields(All);
  FieldListBox.SetFocus;
end;

function TFieldsEditor.DoAddFields(All: Boolean): TField;
var
  AddFields: TAddFields;
  I: Integer;
  FieldName: WideString;
  Field: TField;
begin
  CheckFieldAdd;
  Result := nil;
  try
    DSDesigner.BeginUpdateFieldDefs;
    DataSet.FieldDefs.Update;
  finally
    DSDesigner.EndUpdateFieldDefs;
  end;
  AddFields := TAddFields.Create(Application);
  try
    { Add physical fields not already represented by TField components to the
      to the list of available fields }
    for I := 0 to DataSet.FieldDefList.Count - 1 do
      with Dataset.FieldDefList[I] do
        if (FieldClass <> nil) and not (faHiddenCol in Attributes) then
        begin
          FieldName := DataSet.FieldDefList.Strings[I];
          Field := DataSet.FindField(FieldName);
          if (Field = nil) or (Field.Owner <> Dataset.Owner) then
            AddFields.FieldsList.Items.Add(FieldName);
        end;

    { Show the dialog }
    AddFields.SelectAll;
    AddFields.FieldsList.ItemIndex := 0;
{$IFDEF MSWINDOWS}
    if All or (AddFields.ShowModal <> mrCancel) then
      Result := CreateFields(AddFields.FieldsList);
{$ENDIF}
  finally
    AddFields.Release;
  end;
end;

procedure TFieldsEditor.AddItemClick(Sender: TObject);
begin
  AddFields(False);
end;

procedure TFieldsEditor.DeleteItemClick(Sender: TObject);
begin
  RemoveFields(GetActiveListbox);
end;

procedure TFieldsEditor.FieldListBoxDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Item: Integer;

  procedure DrawRect(Item: Integer);
  begin
    if Item <> -1 then
      with FieldlistBox do
        Canvas.DrawFocusRect(ItemRect(Item));
    FFocusRectItem := Item;
  end;

begin
  Item := FieldListBox.ItemAtPos(Point(X, Y), False);
  Accept := (Source is TDragFields) and
    (TDragFields(Source).Control = FieldListBox) and
    (Item >= 0) and (Item < FieldListBox.Items.Count) and
    not FieldListBox.Selected[Item];
  if State = dsDragEnter then FFocusRectItem := -1;
  if (State = dsDragLeave) or not Accept then Item := -1;
  DrawRect(FFocusRectItem);
  DrawRect(Item);
end;

procedure TFieldsEditor.FieldListBoxDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  F: TField;
  I: Integer;
begin
  if (Source is TDragFields) and (TDragFields(Source).Control = FieldListBox) then
  begin
    try
      DataSet.DisableControls;
      try
        with FieldListBox do
        begin
          F := TField(Items.Objects[ItemAtPos(Point(X, Y), True)]){Dataset.FieldByName(Items[ItemAtPos(Point(X, Y), True)])};
          for I := 0 to Items.Count - 1 do
            if Selected[I] then
              TField(Items.Objects[I]).Index{Dataset.FieldByName(Items[I]).Index} := F.Index;
        end;
      finally
        DataSet.EnableControls;
      end;
    finally
      UpdateDisplay;
      Designer.Modified;
    end;
  end;
end;

procedure TFieldsEditor.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  inherited;
  with Message.MinMaxInfo^.ptMinTrackSize do
  begin
    X := FMinWidth;
    Y := FMinHeight;
  end;
end;

procedure TFieldsEditor.AListBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_INSERT: NewItemClick(Self);
    VK_DELETE: RemoveFields(Sender as TListbox);
    VK_UP:
      if (ssCtrl in Shift) and (Sender = FieldListBox) then MoveFields(-1) else Exit;
    VK_DOWN:
      if (ssCtrl in Shift) and (Sender = FieldListBox) then MoveFields(1) else Exit;
  else
    Exit;
  end;
  Key := 0;
end;

procedure TFieldsEditor.NewItemClick(Sender: TObject);
var
  //DefineField: TDefineField;
  Selection: TWideStringList;
  //Columns: Integer;
  Field: TField;
begin
  CheckFieldAdd;
  Field := DoNewField;
  if Field <> nil then
  begin
    Selection := TWideStringList.Create;
    try
      Selection.Add(Field.FieldName);
    finally
      RestoreSelection(FieldListBox, Selection, -1, -1, False);
    end;
  end;
  FieldListBox.SetFocus;

  {DefineField := TDefineField.Create(Application);
  try
    DefineField.DSDesigner := FDSDesigner;
    DefineField.Designer := Designer;
    DefineField.Dataset := Dataset;
    Columns := 3;
    if DSDesigner.SupportsInternalCalc then
    begin
      DefineField.FieldKind.Items.Add(SFKInternalCalc);
      Inc(Columns);
    end;
    if DSDesigner.SupportsAggregates then
    begin
      DefineField.FieldKind.Items.Add(SFKAggregate);
      Inc(Columns);
    end;
    DefineField.FieldKind.Columns := Columns;
    with DefineField do
      if ShowModal = mrOK then
      begin
        Self.Designer.Modified;
        Self.UpdateDisplay;
        Selection := TWideStringList.Create;
        try
          Selection.Add(FieldName);
        finally
          RestoreSelection(FieldListBox, Selection, -1, -1, False);
        end;
      end;
  finally
    DefineField.Release;
  end;
  FieldListBox.SetFocus;}
end;

function TFieldsEditor.DoNewField: TField;
var
  DefineField: TDefineField;
  //Selection: TWideStringList;
  //Columns: Integer;
begin
  Result := nil;
  DefineField := TDefineField.Create(Application);
  try
    DefineField.DSDesigner := FDSDesigner;
    DefineField.Designer := Designer;
    DefineField.Dataset := Dataset;
    //Columns := 3;
    if DSDesigner.SupportsInternalCalc then
    begin
      DefineField.FieldKind.Items.Add(SFKInternalCalc);
      //Inc(Columns);
    end;
    if DSDesigner.SupportsAggregates then
    begin
      DefineField.FieldKind.Items.Add(SFKAggregate);
      //Inc(Columns);
    end;
    //DefineField.FieldKind.Columns := Columns;
    if DefineField.ShowModal = mrOk then
    begin
      Result := DefineField.Field;
      if Visible then
        UpdateDisplay;
      Designer.Modified;
    end;
  finally
    DefineField.Release;
  end;
end;

function TFieldsEditor.DoNewLookupField(const ADataSet, AKey, ALookup,
  AResult, AType: string; ASize: Word): TField;
var
  DefineField: TDefineField;
  //Selection: TWideStringList;
  //Columns: Integer;
begin
  CheckFieldAdd;
  Result := nil;
  DefineField := TDefineField.Create(Application);
  try
    DefineField.DSDesigner := FDSDesigner;
    DefineField.Designer := Designer;
    DefineField.Dataset := Dataset;
    DefineField.ConfigureForLookupOnly(ADataSet, AKey, ALookup,
                                       AResult, AType, ASize);
    if DefineField.ShowModal = mrOk then
    begin
      Result := DefineField.Field;
      if Visible then
        UpdateDisplay;
      Designer.Modified;
    end;
  finally
    DefineField.Release;
  end;
end;

procedure TFieldsEditor.Activated;
begin
  Designer.Activate;
  try
    UpdateSelection;
  except
    FieldListBox.Items.Clear;
  end;
end;

function TFieldsEditor.UniqueName(Component: TComponent): string;
var
  FullName: string;
begin
  if Component is TField then
    FullName := TField(Component).FullName
  else
    FullName := '';
  Result := CreateUniqueName(Dataset, FullName,
      TFieldClass(Component.ClassType), Component)
end;

function TFieldsEditor.GetEditState: TEditState;

  function FieldsSelected(Listbox: TListbox): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    with ListBox do
      for I := 0 to Items.Count - 1 do
        if Selected[I] then Exit;
    Result := False;
  end;

begin
  Result := [];
  if ClipboardComponents then Result := [esCanPaste];
  if FieldsSelected(FieldListbox) or FieldsSelected(AggListBox) then
    Result := Result + [esCanCopy, esCanCut, esCanDelete];
end;

function TFieldsEditor.EditAction(Action: TEditAction): Boolean;
begin
  Result := True;
  case Action of
    eaCut: Cut;
    eaCopy: Copy;
    eaPaste: Paste;
    eaDelete: RemoveFields(GetActiveListbox);
    eaSelectAll:
      begin
        SelectAll;
        UpdateSelection;
      end;
  else
    Result := False;
  end;
end;

procedure TFieldsEditor.Endupdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and FFieldListUpdated then
      UpdateFieldList;
  end;
end;

procedure TFieldsEditor.Cut;
begin
  CheckFieldDelete;
  Copy;
  RemoveFields(GetActiveListbox);
end;

procedure TFieldsEditor.Copy;
var
  I: Integer;
  ComponentList: IDesignerSelections;
begin
  ComponentList := TDesignerSelections.Create;
  try
    with GetActiveListBox do
      for I := 0 to Items.Count - 1 do
        if Selected[I] then
          ComponentList.Add(TComponent(Items.Objects[I]){Dataset.FieldByName(Items[I])});
    CopyComponents(Dataset.Owner, ComponentList);
  finally
  end;
end;

procedure TFieldsEditor.Paste;
var
  I, Index: Integer;
  ComponentList: IDesignerSelections;
  Field, F: TField;
begin
  ComponentList := TDesignerSelections.Create;
  try
    F := nil;
    with FieldListBox do
      if (ItemIndex <> -1) and (Items.Count > 0) then
        F := TField(Items.Objects[ItemIndex]){Dataset.FieldByName(Items[ItemIndex])};
    try
      BeginUpdate;
      try
        FDSDesigner.BeginDesign;
        try
          PasteComponents(Dataset.Owner, Dataset, ComponentList);
        finally
          FDSDesigner.EndDesign;
        end;
      finally
        EndUpdate;
      end;
    finally
      UpdateDisplay;
    end;
    try
      with FieldListBox do
        for I := 0 to Items.Count - 1 do Selected[I] := False;
      for I := 0 to ComponentList.Count - 1 do
        if ComponentList[I] is TField then
        begin
          Field := TField(ComponentList[I]);
          Field.Name := UniqueName(Field);
          Index := FieldListBox.Items.IndexOf(Field.FullName);
          if Index <> -1 then FieldListBox.Selected[Index] := True;
          if F <> nil then Field.Index := F.Index;
        end;
    finally
      UpdateDisplay;
    end;
  finally
  end;
end;

procedure TFieldsEditor.ItemsModified(const Designer: IDesigner);
begin
  UpdateCaption;
end;

procedure TFieldsEditor.SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections);
var
  I: Integer;
  S: Boolean;

  function InSelection(Component: TComponent): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    if ASelection <> nil then
      with ASelection do
        for I := 0 to Count - 1 do
          if Component = Items[I] then Exit;
    Result := False;
  end;

begin
  with FieldListBox do
    for I := 0 to Items.Count - 1 do
    begin
      S := InSelection(TComponent(Items.Objects[I]));
      if Selected[I] <> S then Selected[I] := S;
    end;
  with AggListBox do
    for I := 0 to Items.Count - 1 do
    begin
      S := InSelection(TComponent(Items.Objects[I]));
      if Selected[I] <> S then Selected[I] := S;
    end;
end;

procedure TFieldsEditor.SelectTable(Sender: TObject);
var
  I: Integer;
begin
  FieldListBox.ItemIndex := 0;
  with FieldListBox do
    for I := 0 to Items.Count - 1 do
      if Selected[I] then Selected[I] := False;
  UpdateSelection;
  FieldListBox.SetFocus;
end;

procedure TFieldsEditor.AListBoxClick(Sender: TObject);
begin
  UpdateSelection;
end;

procedure TFieldsEditor.AListBoxKeyPress(Sender: TObject;
  var Key: Char);
begin
  case Key of
    #13, #33..#126:
      begin
        if Key = #13 then Key := #0;
        ActivateInspector(Key);
        Key := #0;
      end;
    #27:
      begin
        SelectTable(Self);
        Key := #0;
      end;
  end;
end;

procedure TFieldsEditor.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TFieldsEditor.ClearAllClick(Sender: TObject);
begin
  CheckFieldDelete;
  if MessageDlg(SDSConfirmDeleteAll, mtConfirmation, mbOKCancel, 0) <> idCancel then
  begin
    SelectAll;
    RemoveFields(GetActiveListbox);
  end;
end;

procedure TFieldsEditor.FieldListBoxStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  if FieldListBox.Items.Count > 0 then
  begin
    if FDragObject = nil then
      FDragObject := TDragFields.Create(FieldListBox, Self);
    DragObject := FDragObject;
  end;
end;

procedure TFieldsEditor.SelectAllItemClick(Sender: TObject);
begin
  SelectAll;
  UpdateSelection;
end;

procedure TFieldsEditor.CutItemClick(Sender: TObject);
begin
  Cut;
end;

procedure TFieldsEditor.CopyItemClick(Sender: TObject);
begin
  Copy;
end;

procedure TFieldsEditor.PasteItemClick(Sender: TObject);
begin
  Paste;
end;

procedure TFieldsEditor.LocalMenuPopup(Sender: TObject);
var
  EditState: TEditState;
begin
  EditState := GetEditState;
  CopyItem.Enabled := esCanCopy in EditState;
  PasteItem.Enabled := esCanPaste in EditState;
  CutItem.Enabled := esCanCut in EditState;
  DeleteItem.Enabled := esCanDelete in EditState;
  SelectAllItem.Enabled := FieldListBox.Items.Count > 0;
  DSDesigner.UpdateMenus(LocalMenu, EditState);
end;

function TFieldsEditor.ForEachSelection(Proc: TSelectionProc): Boolean;
var
  Field: TField;
  I: Integer;
begin
  Result := False;
  with FieldListBox do
    for I := 0 to Items.Count - 1 do
      if Selected[I] then
      begin
        Field := TField(Items.Objects[I]){Dataset.FindField(Items[I])};
        if (Field <> nil) and not Proc(Field) then Exit;
      end;
  Result := True;
end;

procedure TFieldsEditor.AddAllFields(Sender: TObject);
begin
  AddFields(True);
end;

function TFieldsEditor.GetActiveListbox: TListbox;
begin
  if ActiveControl = AggListbox then
    Result := AggListbox
  else
    Result := FieldListBox;
end;

procedure TFieldsEditor.CheckFieldDelete;
var
  I: Integer;
begin
  with GetActiveListBox do
    for I := 0 to Items.Count-1 do
      if Selected[I] and (csAncestor in TField(Items.Objects[I]).ComponentState) then
        raise Exception.CreateRes(@SCantDeleteAncestor);
end;

procedure TFieldsEditor.CheckFieldAdd;
begin
  if (FDataset <> nil) and (FDataset.Owner <> nil) and
    (csInline in FDataset.Owner.ComponentState) then
    raise Exception.CreateRes(@SCantAddToFrame);
end;

procedure TFieldsEditor.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  if AItem = DataSet then
    DataSet := nil
  else if (AItem is TField) and
          (TField(AItem).DataSet = DataSet) then
    UpdateDisplay;
end;

{$IFDEF MSWINDOWS}
initialization
  RegisterDragTarget(TDragFields.ClassName, TFieldsTarget);
{$ENDIF}
end.

