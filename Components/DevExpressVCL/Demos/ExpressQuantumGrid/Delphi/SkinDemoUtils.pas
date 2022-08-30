unit SkinDemoUtils;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ComCtrls,
{$IFDEF EXPRESSBARS}
  dxBar, dxStatusBar,
{$ENDIF}
  dxCore, cxLookAndFeels;

const
  sdxDefaultSkinName = 'Office2013White';

type
{$IFDEF EXPRESSBARS}
  TdxBaseMenuItem = TdxBarItem;
{$ELSE}
  TdxBaseMenuItem = TMenuItem;
{$ENDIF}

{$IFDEF EXPRESSBARS}
function FindMenuItem(ABarmanager: TdxBarManager; const AMenuItemName: string; out AItem: TdxBaseMenuItem): Boolean;
procedure dxBarConvertMainMenu(AMenu: TMainMenu; ABarManager: TdxBarManager);
{$ELSE}
function FindMenuItem(AMenu: TMainMenu; const AMenuItemName: string; out AItem: TdxBaseMenuItem): Boolean;
{$ENDIF}

procedure SetMenuItemVisible(AMenuItem: TObject; AVisible: Boolean);
procedure SetMenuItemEnable(AMenuItem: TObject; AEnable: Boolean);
procedure SetMenuItemChecked(AMenuItem: TObject; AChecked: Boolean);

function GetMenuItemChecked(AMenuItem: TObject): Boolean;

{$IFDEF EXPRESSBARS}
procedure CreateSkinsMenuItem(ABarmanager: TdxBarManager);
{$ELSE}
procedure CreateSkinsMenuItem(AMenu: TMainMenu);
{$ENDIF}

implementation

uses
{$IFDEF EXPRESSSKINS}
  {$I dxSkins.inc}
  dxSkinsStrs, dxSkinsForm, dxSkinInfo,
{$IFDEF EXPRESSPAGECONTROL}
  dxSkinscxPCPainter,
{$ENDIF}
{$IFDEF EXPRESSBARS}
  dxSkinsdxBarPainter,
  dxSkinsdxStatusBarPainter,
{$IFDEF EXPRESSPAGECONTROL}
  {$IFDEF EXPRESSEDITORS}
    dxBarSkinnedCustForm,
  {$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
  dxGDIPlusAPI, cxLookAndFeelPainters;


var
  FSkinNames: TStringList;
  FSkinResources: TStringList;
{$IFDEF EXPRESSSKINS}
  FSkinController: TdxSkinController;
{$ENDIF}

const
  LevelGroupIndex: Integer = 200;

type
  { TcxLookAndFeelCustomMenuBuilder }

  TcxLookAndFeelCustomMenuBuilder = class(TObject)
  protected
    class procedure MenuItemClickHandler(ASender: TObject); virtual;
  public
    procedure AddButton(AParent: TObject; const ACaption: string; ATag: Integer); virtual; abstract;
    function AddSubItem(AParent: TObject; const ACaption: string): TObject; virtual; abstract;
    function GetSubItemByName(AParent: TObject; const ASubItemCaption: string): TObject; virtual; abstract;
  end;
  TcxLookAndFeelCustomMenuBuilderClass = class of TcxLookAndFeelCustomMenuBuilder;

  { TcxLookAndFeelMenuBuilder }

  TcxLookAndFeelMenuBuilder = class(TcxLookAndFeelCustomMenuBuilder)
  protected
    class procedure MenuItemClickHandler(ASender: TObject); override;
  public
    procedure AddButton(AParent: TObject; const ACaption: string; ATag: Integer); override;
    function AddSubItem(AParent: TObject; const ACaption: string): TObject; override;
    function CreateMenuItem(const ACaption: string; AParent: TMenuItem): TMenuItem;
    function GetSubItemByName(AParent: TObject; const ASubItemCaption: string): TObject; override;
  end;

{$IFDEF EXPRESSBARS}
  { TcxLookAndFeelBarsMenuBuilder }

  TcxLookAndFeelBarsMenuBuilder = class(TcxLookAndFeelCustomMenuBuilder)
  protected
    class procedure MenuItemClickHandler(ASender: TObject); override;
  public
    procedure AddButton(AParent: TObject; const ACaption: string; ATag: Integer); override;
    function AddSubItem(AParent: TObject; const ACaption: string): TObject; override;
    function GetSubItemByName(AParent: TObject; const ASubItemCaption: string): TObject; override;
  end;
{$ENDIF}

procedure PopulateSkinNames;
{$IFDEF EXPRESSSKINS}

  procedure PopulateSkinNamesByResources;
  var
    I: Integer;
  begin
    FSkinResources := TStringList.Create;
    dxSkinsPopulateSkinResources(HInstance, FSkinResources, FSkinNames);
    for I := 0 to FSkinNames.Count - 1 do
      if SameText(FSkinNames[I], sdxDefaultUserSkinData) then
      begin
        FSkinNames.Delete(I);
        Break;
      end;
  end;

  procedure PopulateSkinNamesByExtendedPainters;
  var
    ADefaultSkinIndex, I: Integer;
  begin
    for I := 0 to cxLookAndFeelPaintersManager.Count - 1 do
    begin
      if cxLookAndFeelPaintersManager[I].LookAndFeelStyle = lfsSkin then
        FSkinNames.Add(cxLookAndFeelPaintersManager[I].LookAndFeelName);
    end;
    ADefaultSkinIndex := FSkinNames.IndexOf(sdxSkinsUserSkinName);
    if ADefaultSkinIndex >= 0 then
      FSkinNames.Delete(ADefaultSkinIndex);
  end;

{$ENDIF}
begin
  if FSkinNames = nil then
  begin
    FSkinNames := TStringList.Create;
{$IFDEF EXPRESSSKINS}
  {$IFDEF DXSKINDYNAMICLOADING}
    PopulateSkinNamesByResources;
  {$ELSE}
    PopulateSkinNamesByExtendedPainters;
  {$ENDIF}
{$ENDIF}
  end;
end;

procedure dxBuildLookAndFeelMenu(AParent: TObject;
  AMenuBuilderClass: TcxLookAndFeelCustomMenuBuilderClass); overload;
var
  AMenuBuilder: TcxLookAndFeelCustomMenuBuilder;
  AItem: TObject;
  I: Integer;
begin
  AMenuBuilder := AMenuBuilderClass.Create;
  try
    AMenuBuilder.AddButton(AParent, 'Native', 4);
    AMenuBuilder.AddButton(AParent, 'Standard', 1);
    AMenuBuilder.AddButton(AParent, 'Flat', 0);
    AMenuBuilder.AddButton(AParent, 'UltraFlat', 2);
    AMenuBuilder.AddButton(AParent, 'Office11', 3);
    PopulateSkinNames;
    if FSkinNames.Count > 0 then
    begin
      AParent := AMenuBuilder.AddSubItem(AParent, 'Skins');
      for I := 0 to FSkinNames.Count - 1 do
        AMenuBuilder.AddButton(AParent, FSkinNames[I], I + 5);
    end;
    AItem := AMenuBuilder.GetSubItemByName(AParent, sdxDefaultSkinName);
    if AItem <> nil then
      AMenuBuilder.MenuItemClickHandler(AItem);
  finally
    AMenuBuilder.Free;
  end;
end;

procedure dxBuildLookAndFeelMenu(AMenuItem: TMenuItem); overload;
begin
  dxBuildLookAndFeelMenu(AMenuItem, TcxLookAndFeelMenuBuilder);
end;

{$IFDEF EXPRESSBARS}
procedure dxBuildLookAndFeelMenu(AMenuItem: TdxBarSubItem); overload;
begin
  dxBuildLookAndFeelMenu(AMenuItem, TcxLookAndFeelBarsMenuBuilder);
end;
{$ENDIF}

{ TcxLookAndFeelCustomMenuBuilder }

class procedure TcxLookAndFeelCustomMenuBuilder.MenuItemClickHandler(ASender: TObject);

  procedure SelectLookAndFeelKind(AValue: TcxLookAndFeelKind);
  begin
    RootLookAndFeel.Kind := AValue;
    RootLookAndFeel.NativeStyle := False;
    RootLookAndFeel.SkinName := '';
  end;

{$IFDEF EXPRESSSKINS}

  procedure SelectSkin(ASkinIndex: Integer);
  {$IFDEF DXSKINDYNAMICLOADING}
  var
    AStream: TStream;
  {$ENDIF}
  begin
    RootLookAndFeel.NativeStyle := False;
  {$IFDEF DXSKINDYNAMICLOADING}
    AStream := TResourceStream.Create(hInstance,
      FSkinResources[Integer(FSkinNames.Objects[ASkinIndex])], PChar(sdxResourceType));
    try
      dxSkinsUserSkinLoadFromStream(AStream);
      RootLookAndFeel.SkinName := sdxSkinsUserSkinName;
    finally
      AStream.Free;
    end;
  {$ELSE}
    RootLookAndFeel.SkinName := FSkinNames[ASkinIndex];
  {$ENDIF}
  end;

{$ENDIF}

begin
  case TComponent(ASender).Tag of
    0..3:
      SelectLookAndFeelKind(TcxLookAndFeelKind(TComponent(ASender).Tag));
    4:
      RootLookAndFeel.NativeStyle := True;
  {$IFDEF EXPRESSSKINS}
    else
      SelectSkin(TComponent(ASender).Tag - 5);
  {$ENDIF}
  end;
end;

{ TcxLookAndFeelMenuBuilder }

procedure TcxLookAndFeelMenuBuilder.AddButton(
  AParent: TObject; const ACaption: string; ATag: Integer);
var
  AMenuItem: TMenuItem;
begin
  AMenuItem := CreateMenuItem(ACaption, AParent as TMenuItem);
  AMenuItem.OnClick := MenuItemClickHandler;
  AMenuItem.RadioItem := True;
  AMenuItem.Tag := ATag;
end;

function TcxLookAndFeelMenuBuilder.AddSubItem(
  AParent: TObject; const ACaption: string): TObject;
begin
  Result := CreateMenuItem(ACaption, AParent as TMenuItem);
end;

function TcxLookAndFeelMenuBuilder.CreateMenuItem(
  const ACaption: string; AParent: TMenuItem): TMenuItem;
begin
  Result := TMenuItem.Create(AParent);
  Result.Caption := ACaption;
  AParent.Add(Result);
end;

function TcxLookAndFeelMenuBuilder.GetSubItemByName(AParent: TObject; const ASubItemCaption: string): TObject;
begin
  Result := (AParent as TMenuItem).Find(ASubItemCaption);
end;

class procedure TcxLookAndFeelMenuBuilder.MenuItemClickHandler(ASender: TObject);

  procedure UncheckMenuItems(AParentItem: TMenuItem);
  var
    AMenuItem: TMenuItem;
    I: Integer;
  begin
    for I := 0 to AParentItem.Count - 1 do
    begin
      AMenuItem := AParentItem[I];
      AMenuItem.Checked := AMenuItem = ASender;
      UncheckMenuItems(AMenuItem);
    end;
  end;

begin
  case TComponent(ASender).Tag of
    0..4:
      UncheckMenuItems((ASender as TMenuItem).Parent);
    else
      UncheckMenuItems((ASender as TMenuItem).Parent.Parent);
  end;
  inherited MenuItemClickHandler(ASender);
end;

{$IFDEF EXPRESSBARS}
{ TcxLookAndFeelBarsMenuBuilder }

procedure TcxLookAndFeelBarsMenuBuilder.AddButton(
  AParent: TObject; const ACaption: string; ATag: Integer);
var
  AButton: TdxBarButton;
  AParentSubItem: TdxBarSubItem;
begin
  AParentSubItem := AParent as TdxBarSubItem;
  AButton := AParentSubItem.BarManager.AddButton;
  AButton.OnClick := MenuItemClickHandler;
  AButton.Caption := ACaption;
  AButton.ButtonStyle := bsChecked;
  AButton.GroupIndex := LevelGroupIndex;
  AButton.Tag := ATag;
  AParentSubItem.ItemLinks.Add(AButton);
end;

function TcxLookAndFeelBarsMenuBuilder.AddSubItem(
  AParent: TObject; const ACaption: string): TObject;
var
  AParentSubItem: TdxBarSubItem;
  ASubItem: TdxBarSubItem;
begin
  AParentSubItem := AParent as TdxBarSubItem;
  ASubItem := AParentSubItem.BarManager.AddSubItem;
  ASubItem.Caption := ACaption;
  AParentSubItem.ItemLinks.Add(ASubItem);
  Result := ASubItem;
end;

function TcxLookAndFeelBarsMenuBuilder.GetSubItemByName(AParent: TObject; const ASubItemCaption: string): TObject;
var
  ALinks: TdxBarItemLinks;
  I: Integer;
begin
  Result := nil;
  ALinks := (AParent as TdxBarSubItem).ItemLinks;
  for I := 0 to ALinks.Count - 1 do
    if SameText(ALinks[I].Item.Caption, ASubItemCaption) then
    begin
      Result := ALinks[I].Item;
      Break;
    end;
end;

class procedure TcxLookAndFeelBarsMenuBuilder.MenuItemClickHandler(ASender: TObject);
begin
  (ASender as TdxBarButton).Down := True;
  inherited MenuItemClickHandler(ASender);
end;
{$ENDIF}

{$IFDEF EXPRESSBARS}
function FindMenuItem(ABarmanager: TdxBarManager; const AMenuItemName: string; out AItem: TdxBaseMenuItem): Boolean;
var
  I: Integer;
begin
  Result := False;
  if ABarmanager <> nil then
  begin
    for I := 0 to ABarmanager.ItemCount - 1 do
      if SameText(ABarmanager.Items[I].Name, AMenuItemName) then
      begin
        Result := True;
        AItem := ABarmanager.Items[I];
        Break;
      end;
  end;
end;
{$ELSE}
function FindMenuItem(AMenu: TMainMenu; const AMenuItemName: string; out AItem: TdxBaseMenuItem): Boolean;

  function FindMenuItem(AParent: TMenuItem; out AItem: TdxBaseMenuItem): Boolean;
  var
    I: Integer;
  begin
    Result := SameText(AParent.Name, AMenuItemName);
    if Result then
      AItem := AParent
    else
      for I := 0 to AParent.Count - 1 do
      begin
        Result := FindMenuItem(AParent[I], AItem);
        if Result then Break;
      end;
  end;

begin
  Result := FindMenuItem(AMenu.Items, AItem);
end;
{$ENDIF}

procedure SetMenuItemVisible(AMenuItem: TObject; AVisible: Boolean);

{$IFDEF EXPRESSBARS}
const
  BarVisibility: array[Boolean] of TdxBarItemVisible = (ivNever, ivAlways);
{$ENDIF}

begin
{$IFDEF EXPRESSBARS}
  (AMenuItem as TdxBarItem).Visible := BarVisibility[AVisible];
{$ELSE}
  (AMenuItem as TMenuItem).Visible := AVisible;
{$ENDIF}
end;

procedure SetMenuItemEnable(AMenuItem: TObject; AEnable: Boolean);
begin
{$IFDEF EXPRESSBARS}
  (AMenuItem as TdxBarItem).Enabled := AEnable;
{$ELSE}
  (AMenuItem as TMenuItem).Enabled := AEnable;
{$ENDIF}
end;

procedure SetMenuItemChecked(AMenuItem: TObject; AChecked: Boolean);
begin
{$IFDEF EXPRESSBARS}
  (AMenuItem as TdxBarButton).Down := AChecked;
{$ELSE}
  (AMenuItem as TMenuItem).Checked := AChecked;
{$ENDIF}
end;

function GetMenuItemChecked(AMenuItem: TObject): Boolean;
begin
{$IFDEF EXPRESSBARS}
  Result := (AMenuItem as TdxBarButton).Down;
{$ELSE}
  Result := (AMenuItem as TMenuItem).Checked;
{$ENDIF}
end;

{$IFDEF EXPRESSBARS}
procedure CreateSkinsMenuItem(ABarmanager: TdxBarManager);
{$ELSE}
procedure CreateSkinsMenuItem(AMenu: TMainMenu);
{$ENDIF}
var
  AMenuItem: {$IFDEF EXPRESSBARS}TdxBarItemLink{$ELSE}TMenuItem{$ENDIF};
  ACaption: string;
begin
  ACaption := 'Look&&&Feel';
{$IFDEF EXPRESSBARS}
  AMenuItem := ABarmanager.MainMenuBar.ItemLinks.AddItem(TdxBarSubItem);
  AMenuItem.UserCaption := ACaption;
  AMenuItem.Index := 1;
  dxBuildLookAndFeelMenu(AMenuItem.Item as TdxBarSubItem);
{$ELSE}
  AMenuItem := NewItem(ACaption, 0, False, True, nil, 0, 'miSkins');
  AMenu.Items.Insert(1, AMenuItem);
  dxBuildLookAndFeelMenu(AMenuItem);
{$ENDIF}
end;

{$IFDEF EXPRESSBARS}
procedure dxBarConvertMainMenu(AMenu: TMainMenu; ABarManager: TdxBarManager);

  function AddCategory(AName: string): Integer;
  var
    ACategoryName: string;
  begin
    with ABarManager.Categories do
    begin
      ACategoryName := AName;
      Result := IndexOf(ACategoryName);
      if Result < 0 then
        Result := Add(ACategoryName);
    end;
  end;

  function ConvertItem(AItemLinks: TdxBarItemLinks; AMenuItem: TMenuItem;
    ACategory: Integer; ANeedCreateCategory: Boolean): TdxBarItem;
  var
    ABarItem, AChildItem: TdxBarItem;
    AItemLink: TdxBarItemLink;
    ABeginGroup: Boolean;
    AMenuItemName: string;
  begin
    if AMenuItem.Parent = nil then
      ABarItem := nil
    else
    begin
      if AMenuItem.Count = 0 then
        ABarItem := TdxBarButton.Create(ABarManager.Owner)
      else
      begin
        ABarItem := TdxBarSubItem.Create(ABarManager.Owner);
        Inc(LevelGroupIndex);
      end;
      ABarItem.Category := ACategory;
    end;

    with AMenuItem do
    begin
      if ABarItem <> nil then
      begin
        ABarItem.Action := Action;
        ABarItem.ImageIndex := ImageIndex;
        ABarItem.Caption := Caption;
        ABarItem.Enabled := Enabled;
        ABarItem.HelpContext := HelpContext;
        SetMenuItemVisible(ABarItem, Visible);
        ABarItem.Hint := Hint;
        ABarItem.ShortCut := ShortCut;
        ABarItem.Tag := Tag;
        ABarItem.OnClick := OnClick;
        if ABarItem is TdxBarButton then
          with TdxBarButton(ABarItem) do
          begin
            if Checked or RadioItem or AutoCheck then
              ButtonStyle := bsChecked;
            if RadioItem then
            begin
              if AMenuItem.GroupIndex <> 0 then
                GroupIndex := AMenuItem.GroupIndex
              else
                GroupIndex := LevelGroupIndex;
            end;
            Down := Checked;
          end;

        if (Count > 0) and ANeedCreateCategory then
          ACategory := AddCategory(GetTextOf(Caption));
      end;

      ABeginGroup := False;
      while Count > 0 do
        if Items[0].Caption = '-' then
        begin
          ABeginGroup := True;
          Items[0].Free;
        end
        else
        begin
          AChildItem := ConvertItem(AItemLinks, Items[0], ACategory, (Parent = nil) and (AMenu is TMainMenu));
          if Parent = nil then
            AItemLink := AItemLinks.Add
          else
            AItemLink := TdxBarSubItem(ABarItem).ItemLinks.Add;
          AItemLink.Item := AChildItem;
          if ABeginGroup then
          begin
            AItemLink.BeginGroup := True;
            ABeginGroup := False;
          end;
        end;
    end;

    AMenuItemName := AMenuItem.Name;
    if AMenuItem.Parent <> nil then
      AMenuItem.Free;
    if ABarItem <> nil then
      ABarItem.Name := AMenuItemName;

    Result := ABarItem;
  end;

var
  ACategoryName, APopupMenuName: string;
  ACategory: Integer;
  ABarPopupMenu: TdxBarPopupMenu;
  AItemLinks: TdxBarItemLinks;
begin
  Screen.Cursor := crHourGlass;
  try
    ACategoryName := 'Menus';
    ABarPopupMenu := nil;
    if ABarManager.MainMenuBar = nil then
      with ABarManager.Bars.Add do
      begin
        Caption := 'Main Menu';
        DockingStyle := dsTop;
        IsMainMenu := True;
        Visible := True;
      end;
    AItemLinks := ABarManager.MainMenuBar.ItemLinks;
    if ABarManager.Images = nil then
      ABarManager.Images := AMenu.Images;

    ACategory := AddCategory(ACategoryName);

    ABarManager.BeginUpdate;
    try
      ConvertItem(AItemLinks, AMenu.Items, ACategory, False);
    finally
      ABarManager.EndUpdate;
    end;

    AMenu.Free;
    ABarManager.Categories.Move(ACategory, ABarManager.Categories.Count - 1);
    if ABarPopupMenu <> nil then
      ABarPopupMenu.Name := APopupMenuName;
    Inc(LevelGroupIndex);
  finally
    Screen.Cursor := crDefault;
  end;
end;
{$ENDIF}

procedure RegisterAssistants;
begin
{$IFDEF EXPRESSSKINS}
  FSkinController := TdxSkinController.Create(nil);
{$ENDIF}
end;

procedure UnregisterAssistants;
begin
{$IFDEF EXPRESSSKINS}
  FreeAndNil(FSkinController);
{$ENDIF}
  FreeAndNil(FSkinResources);
  FreeAndNil(FSkinNames);
end;

initialization
  dxUnitsLoader.AddUnit(@RegisterAssistants, @UnregisterAssistants);

finalization
  dxUnitsLoader.RemoveUnit(@UnregisterAssistants);

end.
