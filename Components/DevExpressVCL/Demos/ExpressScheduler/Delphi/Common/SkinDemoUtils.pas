unit SkinDemoUtils;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ComCtrls, dxCore, cxLookAndFeels;

const
  sdxDefaultSkinName = 'DevExpressStyle';

type
  TdxBaseMenuItem = TMenuItem;

function FindMenuItem(AMenu: TMainMenu; const AMenuItemName: string; out AItem: TdxBaseMenuItem): Boolean;
procedure SetMenuItemVisible(AMenuItem: TObject; AVisible: Boolean);
procedure SetMenuItemEnable(AMenuItem: TObject; AEnable: Boolean);
procedure SetMenuItemChecked(AMenuItem: TObject; AChecked: Boolean);
function GetMenuItemChecked(AMenuItem: TObject): Boolean;
procedure CreateSkinsMenuItem(AMenu: TMainMenu; APosition: Integer);

implementation

uses
{$IFDEF EXPRESSSKINS}
  {$I dxSkins.inc}
  dxSkinsStrs, dxSkinsForm, dxSkinInfo,
{$IFDEF EXPRESSPAGECONTROL}
  dxSkinscxPCPainter,
{$ENDIF}
{$IFDEF EXPRESSPAGECONTROL}
  {$IFDEF EXPRESSEDITORS}
    dxBarSkinnedCustForm,
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

procedure SetMenuItemVisible(AMenuItem: TObject; AVisible: Boolean);
begin
  (AMenuItem as TMenuItem).Visible := AVisible;
end;

procedure SetMenuItemEnable(AMenuItem: TObject; AEnable: Boolean);
begin
  (AMenuItem as TMenuItem).Enabled := AEnable;
end;

procedure SetMenuItemChecked(AMenuItem: TObject; AChecked: Boolean);
begin
  (AMenuItem as TMenuItem).Checked := AChecked;
end;

function GetMenuItemChecked(AMenuItem: TObject): Boolean;
begin
  Result := (AMenuItem as TMenuItem).Checked;
end;

procedure CreateSkinsMenuItem(AMenu: TMainMenu; APosition: Integer);
var
  AMenuItem: TMenuItem;
  ACaption: string;
begin
{$IFDEF EXPRESSSKINS}
  ACaption := '&Skins';
{$ELSE}
  ACaption := 'LookAnd&Feel';
{$ENDIF}
  AMenuItem := NewItem(ACaption, 0, False, True, nil, 0, 'miSkins');
  AMenu.Items.Insert(APosition, AMenuItem);
  dxBuildLookAndFeelMenu(AMenuItem);
end;

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
