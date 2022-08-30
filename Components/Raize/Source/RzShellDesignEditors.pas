{===============================================================================
  RzShellCtrls Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzShellLocatorProperty
  TCSIDLProperty
  TRzShellDialogEditor
    Adds context menu to text Shell Dialog components.

  TRzShellListEditor
  TRzShellTreeEditor
  TRzShellComboEditor


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial inclusion in Raize Components.
===============================================================================}

{$I RzComps.inc}

{$RANGECHECKS OFF}
{$WRITEABLECONST OFF}
{$TYPEDADDRESS ON}


unit RzShellDesignEditors;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Menus,
  Dialogs,
  StdCtrls,
  ComCtrls,
  DesignIntf,
  DesignEditors,
  VCLEditors,
  DesignMenus,
  RzDesignEditors,
  RzShellDialogs,
  RzLabel,
  RzShellCtrls,
  RzShellUtils, Mask, RzEdit, RzCmboBx, RzRadChk, RzButton;

type
  {===============================================}
  {== TRzShellLocatorProperty Class Declaration ==}
  {===============================================}

  TRzShellLocatorProperty = class( TClassProperty )
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


  {======================================}
  {== TCSIDLProperty Class Declaration ==}
  {======================================}

  TCSIDLProperty = class( TEnumProperty )
  public
    procedure GetValues( Proc: TGetStrProc ); override;
  end;


  {============================================}
  {== TRzShellDialogEditor Class Declaration ==}
  {============================================}

  TRzShellDialogEditor = class( TRzDefaultEditor )
  public
    function GetVerbCount: Integer;  override;
    function GetVerb( Index: Integer ): string;  override;
    procedure ExecuteVerb( Index: Integer );  override;
  end;


  {==========================================}
  {== TRzShellListEditor Class Declaration ==}
  {==========================================}

  TRzShellListEditor = class( TRzDefaultEditor )
  protected
    function ShellList: TRzShellList;
    function AlignMenuIndex: Integer; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure ViewStyleMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
  end;


  {==========================================}
  {== TRzShellTreeEditor Class Declaration ==}
  {==========================================}

  TRzShellTreeEditor = class( TRzComponentEditor )
  protected
    function ShellTree: TRzShellTree;
    function AlignMenuIndex: Integer; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure ShellListMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
  end;


  {===========================================}
  {== TRzShellComboEditor Class Declaration ==}
  {===========================================}

  TRzShellComboEditor = class( TRzComponentEditor )
  protected
    function ShellCombo: TRzShellCombo;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure ShellListMenuHandler( Sender: TObject );
    procedure ShellTreeMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
  end;

  {==============================================}
  {== TRzShellLocatorEditDlg Class Declaration ==}
  {==============================================}

  TRzShellLocatorEditDlg = class(TForm)
    OkBtn: TRzButton;
    CancelBtn: TRzButton;
    UsePidlRdo: TRzRadioButton;
    DontUseAnythingRdo: TRzRadioButton;
    UsePathnameRdo: TRzRadioButton;
    UseCSIDLRdo: TRzRadioButton;
    RzSelectFolderDialog1: TRzSelectFolderDialog;
    ComboBox1: TRzComboBox;
    PathNameEdt: TRzEdit;
    ItemTxt: TRzLabel;
    BrowseBtn: TRzButton;
    procedure UsePidlRdoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BrowseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FLocator: TRzShellLocator;
    procedure SetLocator( Value: TRzShellLocator );
  public
    property Locator: TRzShellLocator
      read FLocator
      write SetLocator;
  end;


function GetCSIDLStr( id: TCSIDL ): string;

implementation

{$R *.dfm}

uses
  TypInfo;

{=====================================}
{== TRzShellLocatorProperty Methods ==}
{=====================================}

function TRzShellLocatorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [ paDialog, paReadOnly ];
end;


procedure TRzShellLocatorProperty.Edit;
var
  F: TRzShellLocatorEditDlg;
begin
  F := TRzShellLocatorEditDlg.Create( Application );
  try
    F.Locator := TRzShellLocator( GetOrdValue );
    if F.ShowModal = mrOk then
      Modified;
  finally
    F.Free;
  end;
end;


{============================}
{== TCSIDLProperty Methods ==}
{============================}

procedure TCSIDLProperty.GetValues( Proc: TGetStrProc );
var
  I: Integer;
  EnumType: PTypeInfo;
  S: string;
begin
  EnumType := GetPropType;
  with GetTypeData(enumType)^ do
    for I := MinValue to MaxValue do
    begin
      S := GetEnumName( enumType, I );
      if Pos( '_', s ) = 0 then
        Proc( S );
    end;
end;


{==================================}
{== TRzShellDialogEditor Methods ==}
{==================================}

function TRzShellDialogEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


function TRzShellDialogEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Preview Dialog';
  end;
end;


procedure TRzShellDialogEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    0: ( Component as TRzShellDialog ).Execute;
  end;
end;



{================================}
{== TRzShellListEditor Methods ==}
{================================}

function TRzShellListEditor.ShellList: TRzShellList;
begin
  // Helper function to provide quick access to component being edited. Also makes sure Component is a TRzListView.
  Result := Component as TRzShellList;
end;


function TRzShellListEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;


function TRzShellListEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Align';
    1: Result := '-';
    2: Result := 'View Style';
  end;
end;


function TRzShellListEditor.AlignMenuIndex: Integer;
begin
  Result := 0;
end;


procedure TRzShellListEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );

  procedure CreateViewStyleMenu( Style: TViewStyle; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Style );
    NewItem.Checked := ShellList.ViewStyle = Style;
    NewItem.OnClick := ViewStyleMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited PrepareMenuItem( Index, Item );

  if Index = 2 then
  begin
    CreateViewStyleMenu( vsIcon, 'Icons' );
    CreateViewStyleMenu( vsSmallIcon, 'Small Icons' );
    CreateViewStyleMenu( vsList, 'List' );
    CreateViewStyleMenu( vsReport, 'Report' );
  end;
end;


procedure TRzShellListEditor.ViewStyleMenuHandler( Sender: TObject );
begin
  ShellList.ViewStyle := TViewStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;



{================================}
{== TRzShellTreeEditor Methods ==}
{================================}

function TRzShellTreeEditor.ShellTree: TRzShellTree;
begin
  // Helper function to provide quick access to component being edited. Also makes sure Component is a TRzTreeView.
  Result := Component as TRzShellTree;
end;


function TRzShellTreeEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;


function TRzShellTreeEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Align';
    1: Result := '-';
    2: Result := 'Set ShellList';
  end;
end;


function TRzShellTreeEditor.AlignMenuIndex: Integer;
begin
  Result := 0;
end;


procedure TRzShellTreeEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
var
  I, ShellListCount: Integer;
  CompOwner: TComponent;

  procedure CreateShellListMenu( ShellList: TRzShellList );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    NewItem.Caption := ShellList.Name;
    NewItem.Checked := GetObjectProp( Component, 'ShellList' ) = ShellList;
    NewItem.OnClick := ShellListMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited PrepareMenuItem( Index, Item );

  case Index of
    2:
    begin
      Item.AutoHotkeys := maManual;
      ShellListCount := 0;
      CompOwner := Designer.GetRoot;
      if CompOwner <> nil then
      begin
        for I := 0 to CompOwner.ComponentCount - 1 do
        begin
          if CompOwner.Components[ I ] is TRzShellList then
          begin
            Inc( ShellListCount );
            CreateShellListMenu( TRzShellList( CompOwner.Components[ I ] ) );
          end;
        end;
      end;
      Item.Enabled := ShellListCount > 0;
    end;
  end;
end;


procedure TRzShellTreeEditor.ShellListMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    ShellTree.ShellList := Designer.GetRoot.FindComponent( S ) as TRzShellList;
    DesignerModified;
  end;
end;



{================================}
{== TRzShellComboEditor Methods ==}
{================================}

function TRzShellComboEditor.ShellCombo: TRzShellCombo;
begin
  // Helper function to provide quick access to component being edited. Also makes sure Component is a TRzTreeView.
  Result := Component as TRzShellCombo;
end;


function TRzShellComboEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;


function TRzShellComboEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Set ShellTree';
    1: Result := 'Set ShellList';
  end;
end;


procedure TRzShellComboEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
var
  I, ShellListCount, ShellTreeCount: Integer;
  CompOwner: TComponent;

  procedure CreateShellTreeMenu( ShellTree: TRzShellTree );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    NewItem.Caption := ShellTree.Name;
    NewItem.Checked := GetObjectProp( Component, 'ShellTree' ) = ShellTree;
    NewItem.OnClick := ShellTreeMenuHandler;
    Item.Add( NewItem );
  end;

  procedure CreateShellListMenu( ShellList: TRzShellList );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    NewItem.Caption := ShellList.Name;
    NewItem.Checked := GetObjectProp( Component, 'ShellList' ) = ShellList;
    NewItem.OnClick := ShellListMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited PrepareMenuItem( Index, Item );

  case Index of
    0:
    begin
      Item.AutoHotkeys := maManual;
      ShellTreeCount := 0;
      CompOwner := Designer.GetRoot;
      if CompOwner <> nil then
      begin
        for I := 0 to CompOwner.ComponentCount - 1 do
        begin
          if CompOwner.Components[ I ] is TRzShellTree then
          begin
            Inc( ShellTreeCount );
            CreateShellTreeMenu( TRzShellTree( CompOwner.Components[ I ] ) );
          end;
        end;
      end;
      Item.Enabled := ShellTreeCount > 0;
    end;

    1:
    begin
      Item.AutoHotkeys := maManual;
      ShellListCount := 0;
      CompOwner := Designer.GetRoot;
      if CompOwner <> nil then
      begin
        for I := 0 to CompOwner.ComponentCount - 1 do
        begin
          if CompOwner.Components[ I ] is TRzShellList then
          begin
            Inc( ShellListCount );
            CreateShellListMenu( TRzShellList( CompOwner.Components[ I ] ) );
          end;
        end;
      end;
      Item.Enabled := ShellListCount > 0;
    end;
  end;
end;


procedure TRzShellComboEditor.ShellTreeMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    ShellCombo.ShellTree := Designer.GetRoot.FindComponent( S ) as TRzShellTree;
    DesignerModified;
  end;
end;


procedure TRzShellComboEditor.ShellListMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    ShellCombo.ShellList := Designer.GetRoot.FindComponent( S ) as TRzShellList;
    DesignerModified;
  end;
end;


{====================================}
{== TRzShellLocatorEditDlg Methods ==}
{====================================}

type
  TItemRec = record
    s: string;
    c: TCSIDL;
  end;


const
  g_sItems: array[0..44] of TItemRec = (
                  (s:'csidlDesktop';                    c:csidlDesktop ),
                  (s:'csidlPrograms';                   c:csidlPrograms ),
                  (s:'csidlControls (Control Panel)';   c:csidlControls ),
                  (s:'csidlPrinters';                   c:csidlPrinters ),
                  (s:'csidlPersonal';                   c:csidlPersonal ),
                  (s:'csidlFavorites';                  c:csidlFavorites ),
                  (s:'csidlStartup';                    c:csidlStartup ),
                  (s:'csidlRecent';                     c:csidlRecent ),
                  (s:'csidlSendTo';                     c:csidlSendTo ),
                  (s:'csidlBitBucket (Recyle Bin)';     c:csidlBitBucket ),
                  (s:'csidlStartMenu';                  c:csidlStartMenu ),
                  (s:'csidlDesktopDirectory';           c:csidlDesktopDirectory ),
                  (s:'csidlDrives (My Computer)';       c:csidlDrives ),
                  (s:'csidlNetwork (Net. Neighborhood)'; c:csidlNetwork ),
                  (s:'csidlNethood';                    c:csidlNethood ),
                  (s:'csidlFonts';                      c:csidlFonts ),
                  (s:'csidlTemplates';                  c:csidlTemplates ),
                  (s:'csidlCommonStartMenu';            c:csidlCommonStartMenu ),
                  (s:'csidlCommonPrograms';             c:csidlCommonPrograms ),
                  (s:'csidlCommonStartup';              c:csidlCommonStartup ),
                  (s:'csidlCommonDesktopDirectory';     c:csidlCommonDesktopDirectory ),
                  (s:'csidlAppData';                    c:csidlAppData ),
                  (s:'csidlPrintHood';                  c:csidlPrintHood ),
                  (s:'csidlLocalAppData';               c:csidlLocalAppData ),
                  (s:'csidlAltStartup';                 c:csidlAltStartup ),
                  (s:'csidlCommonAltStartup';           c:csidlCommonAltStartup ),
                  (s:'csidlCommonFavorites';            c:csidlCommonFavorites ),
                  (s:'csidlInternetCache';              c:csidlInternetCache ),
                  (s:'csidlCookies';                    c:csidlCookies ),
                  (s:'csidlHistory';                    c:csidlHistory ),
                  (s:'csidlCommonAppData';              c:csidlCommonAppData ),
                  (s:'csidlWindows';                    c:csidlWindows ),
                  (s:'csidlSystem';                     c:csidlSystem ),
                  (s:'csidlProgramFiles';               c:csidlProgramFiles ),
                  (s:'csidlMyPictures';                 c:csidlMyPictures ),
                  (s:'csidlProfile';                    c:csidlProfile ),
                  (s:'csidlSystemX86';                  c:csidlSystemX86 ),
                  (s:'csidlProgramFilesX86';            c:csidlProgramFilesX86 ),
                  (s:'csidlProgramFilesCommon';         c:csidlProgramFilesCommon ),
                  (s:'csidlProgramFilesCommonX86';      c:csidlProgramFilesCommonX86 ),
                  (s:'csidlCommonTemplates';            c:csidlCommonTemplates ),
                  (s:'csidlCommonDocuments';            c:csidlCommonDocuments ),
                  (s:'csidlCommonAdminTools';           c:csidlCommonAdminTools ),
                  (s:'csidlAdminTools';                 c:csidlAdminTools ),
                  (s:'csidlConnections';                c:csidlConnections ) );


procedure TRzShellLocatorEditDlg.FormCreate( Sender: TObject );
var i: Integer;
begin
  for i := Low( g_sItems ) to High( g_sItems ) do
    Combobox1.Items.Add( g_sItems[ i ].s );
end;


procedure TRzShellLocatorEditDlg.SetLocator( Value: TRzShellLocator );
var
  i: Integer;
  s: string;
begin
  FLocator := Value;
  PathNameEdt.Text := FLocator.Pathname;
//  if (i > High(g_sItems)) then Combobox1.ItemIndex := -1;  //?
  if Assigned(FLocator) then
    if (FLocator.CSIDL <> csidlNone) then
    begin
      for i := 0 to High(g_sItems) do
        if g_sItems[i].c = FLocator.CSIDL then
        begin
          Combobox1.ItemIndex := i;
          Break;
        end;
      UseCSIDLRdo.Checked := TRUE
    end
    else if Assigned(FLocator.IdList) then
    begin
      s := FLocator.PathName;
      if (s <> '') then
      begin
        PathNameEdt.Text := s;
        UsePathnameRdo.Checked := TRUE;
      end
      else
      begin
        ItemTxt.Caption := ShellGetFriendlyNameFromIdList( nil, FLocator.IdList, fnNormal );
        UsePidlRdo.Checked := TRUE
      end;
    end
    else
      DontUseAnythingRdo.Checked := TRUE;
end; {= TFrmShellLocatorEditor.SetLocator =}


procedure TRzShellLocatorEditDlg.UsePidlRdoClick( Sender: TObject );
const
  _VALS: array[Boolean] of TColor = (clBtnFace, clWhite);
var
  f: Bool;
begin
  f := UsePathnameRdo.Checked;
  PathNameEdt.Enabled := f;
  PathNameEdt.Color := _VALS[f];

  f := UsePidlRdo.Checked;
  BrowseBtn.Enabled := f;
  ItemTxt.Enabled := f;

  f := UseCsidlRdo.Checked;
  Combobox1.Enabled := f;
  Combobox1.Color := _VALS[f];
  if f and (Combobox1.ItemIndex < 0) then
    Combobox1.ItemIndex := 0;
end;


procedure TRzShellLocatorEditDlg.FormClose( Sender: TObject; var Action: TCloseAction );
begin
  inherited;
  if (ModalResult = mrOk) and Assigned(FLocator) then
  begin
    if DontUseAnythingRdo.Checked then
      FLocator.IdList := nil
    else if UsePathnameRdo.Checked then
      FLocator.Pathname := PathNameEdt.Text
    else if UsePidlRdo.Checked then
    begin
      if Assigned( RzSelectFolderDialog1.SelectedFolder.IdList ) then
        FLocator.IdList := RzSelectFolderDialog1.SelectedFolder.IdList;
    end
    else
      FLocator.CSIDL := g_sItems[Combobox1.ItemIndex].c;
  end;
end;


procedure TRzShellLocatorEditDlg.BrowseBtnClick(Sender: TObject);
begin
  if RzSelectFolderDialog1.Execute then
    ItemTxt.Caption := ShellGetFriendlyNameFromIdList( nil, RzSelectFolderDialog1.SelectedFolder.IdList, fnNormal );
end;



function GetCSIDLStr( id: TCSIDL ): string;
var
  i: Integer;
begin
  for i := 0 to High(g_sItems) do
    if g_sItems[i].c = id then
    begin
      Result := g_sItems[i].s;
      Exit;
    end;
  Result := '';
end;

end.

