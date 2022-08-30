{===============================================================================
  RzGroupBarEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzGroupBarEditor
    Allows user to add groups to a TRzGroupBar at design-time.


  Modification History
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Fixed problem where group items with a Caption of '-' appeared as a
      separator line rather than a menu item in the "Edit Item" context menu.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * Fixed problem where adding an item to a group did not call
      DesignerModified.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzGroupBarEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Menus,
  Forms,
  Classes,
  StdCtrls,
  RzGroupBar,
  DesignIntf,
  DesignEditors,
  DesignMenus,
  RzDesignEditors,
  Controls,
  RzLabel, ExtCtrls, RzPanel;

type
  TRzGroupBarEditor = class( TRzComponentEditor )
  protected
    function GroupBar: TRzGroupBar;
    function AlignMenuIndex: Integer; override;
    function GetCompRefData( Index: Integer; var CompRefClass: TComponentClass; var CompRefPropName: string;
                             var CompRefMenuHandler: TNotifyEvent ): Boolean; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure GroupBarStyleMenuHandler( Sender: TObject );
    procedure VisualStyleMenuHandler( Sender: TObject );
    procedure GradientColorStyleMenuHandler( Sender: TObject );
    procedure SmallImagesMenuHandler( Sender: TObject );
    procedure LargeImagesMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
    procedure Edit; override;
  end;


  TRzGroupEditor = class( TRzComponentEditor )
  protected
    function Group: TRzGroup;
    function GetCompRefData( Index: Integer; var CompRefClass: TComponentClass; var CompRefPropName: string;
                             var CompRefMenuHandler: TNotifyEvent ): Boolean; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure ActionListMenuHandler( Sender: TObject );
    procedure ItemsMenuHandler( Sender: TObject );
    procedure ItemStyleMenuHandler( Sender: TObject );
    procedure GroupBarStyleMenuHandler( Sender: TObject );
    procedure VisualStyleMenuHandler( Sender: TObject );
    procedure GradientColorStyleMenuHandler( Sender: TObject );
    procedure SmallImagesMenuHandler( Sender: TObject );
    procedure LargeImagesMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
    procedure Edit; override;
  end;


  TRzGroupControllerEditor = class( TRzComponentEditor )
  protected
    function GroupController: TRzGroupController;
    function GetCompRefData( Index: Integer; var CompRefClass: TComponentClass; var CompRefPropName: string;
                             var CompRefMenuHandler: TNotifyEvent ): Boolean; override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
    procedure Edit; override;
  end;


  TRzGroupTemplateEditor = class( TRzComponentEditor )
  protected
    function Template: TRzGroupTemplate;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure ActionListMenuHandler( Sender: TObject );
    procedure SmallImagesMenuHandler( Sender: TObject );
    procedure LargeImagesMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
    procedure Edit; override;
  end;

  {========================================}
  {== TRzGroupDesigner Class Declaration ==}
  {========================================}

  {$IFDEF USE_GROUP_DESIGNER}

  TRzGroupDesigner = class( TComponent, IDesignNotification, IRzGroupDesigner )
  private
    FDesigner: IDesigner;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure ItemDeleted( const ADesigner: IDesigner; AItem: TPersistent );
    procedure ItemInserted( const ADesigner: IDesigner; AItem: TPersistent );
    procedure ItemsModified( const ADesigner: IDesigner );
    procedure SelectionChanged( const ADesigner: IDesigner; const ASelection: IDesignerSelections );
    procedure DesignerOpened( const ADesigner: IDesigner; AResurrecting: Boolean );
    procedure DesignerClosed( const ADesigner: IDesigner; AGoingDormant: Boolean );

    procedure SetSelection( APersistent: TPersistent );
  end;

  {$ENDIF}

  TRzGroupTemplatePreviewDlg = class(TForm)
    GroupBar: TRzGroupBar;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    procedure ItemClickHandler( Sender: TObject );
  public
    procedure SetTemplate( Template: TRzGroupTemplate );
  end;


{$IFDEF USE_GROUP_DESIGNER}
procedure Register;
{$ENDIF}


implementation

{$R *.dfm}

uses
  ImgList,
  Windows,
  ActnList,
  RzCommon;


{===============================}
{== TRzGroupBarEditor Methods ==}
{===============================}

function TRzGroupBarEditor.GroupBar: TRzGroupBar;
begin
  Result := Component as TRzGroupBar;
end;


function TRzGroupBarEditor.GetVerbCount: Integer;
begin
  Result := 10;
end;


function TRzGroupBarEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Add Group';
    1: Result := '-';
    2: Result := 'GroupBar Style';
    3: Result := 'Visual Style';
    4: Result := 'Gradient Color Style';
    5: Result := '-';
    6: Result := 'Set Small ImageList';
    7: Result := 'Set Large ImageList';
    8: Result := '-';
    9: Result := 'Align';
  end;
end;


function TRzGroupBarEditor.AlignMenuIndex: Integer;
begin
  Result := 9;
end;


function TRzGroupBarEditor.GetCompRefData( Index: Integer; var CompRefClass: TComponentClass;
                                           var CompRefPropName: string;
                                           var CompRefMenuHandler: TNotifyEvent ): Boolean;
begin
  Result := False;
  case Index of
    6:
    begin
      CompRefClass := TCustomImageList;
      CompRefPropName := 'SmallImages';
      CompRefMenuHandler := SmallImagesMenuHandler;
      Result := True;
    end;

    7:
    begin
      CompRefClass := TCustomImageList;
      CompRefPropName := 'LargeImages';
      CompRefMenuHandler := LargeImagesMenuHandler;
      Result := True;
    end;
  end;
end;


procedure TRzGroupBarEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );

  procedure CreateGroupBarStyleMenu( Style: TRzGroupBarStyle; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Style );
    NewItem.Checked := GroupBar.Style = Style;
    NewItem.OnClick := GroupBarStyleMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  case Index of
    0:
    begin
      // Only allow user to add new groups if the group bar is
      // NOT being edited in an inline frame (i.e. a frame instance).
      Item.Enabled := not IsInInlined;
    end;

    2: // GroupBar Style
    begin
      CreateGroupBarStyleMenu( gbsCategoryView, 'Category View' );
      CreateGroupBarStyleMenu( gbsTaskList, 'Task List' );
      CreateGroupBarStyleMenu( gbsOutlook, 'Outlook' );
    end;

    3: // Visual Style
    begin
      CreateVisualStyleMenuItem( Item, vsClassic, GroupBar.VisualStyle,
                                 VisualStyleMenuHandler );
      CreateVisualStyleMenuItem( Item, vsWinXP, GroupBar.VisualStyle,
                                 VisualStyleMenuHandler );
      CreateVisualStyleMenuItem( Item, vsGradient, GroupBar.VisualStyle,
                                 VisualStyleMenuHandler );
    end;

    4: // Gradient Color Style
    begin
      CreateGradientColorStyleMenuItem( Item, gcsSystem, GroupBar.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
      CreateGradientColorStyleMenuItem( Item, gcsMSOffice, GroupBar.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
      CreateGradientColorStyleMenuItem( Item, gcsCustom, GroupBar.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
    end;
  end;
end;


procedure TRzGroupBarEditor.ExecuteVerb( Index: Integer );
var
  G: TRzGroup;
begin
  case Index of
    0:
    begin
      // Calling Designer.CreateComponent does not work b/c is causes the group to be first parented by the form
      G := GroupBar.GroupClass.Create( Designer.GetRoot );
      G.Name := GetNewComponentName( Designer.GetRoot, 'RzGroup', False );
      GroupBar.AddGroup( G );
      DesignerModified;
      Designer.SelectComponent( G );
    end;
  end;
end;


procedure TRzGroupBarEditor.GroupBarStyleMenuHandler( Sender: TObject );
begin
  GroupBar.Style := TRzGroupBarStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzGroupBarEditor.VisualStyleMenuHandler( Sender: TObject );
begin
  GroupBar.VisualStyle := TRzVisualStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzGroupBarEditor.GradientColorStyleMenuHandler( Sender: TObject );
begin
  GroupBar.GradientColorStyle := TRzGradientColorStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzGroupBarEditor.SmallImagesMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    GroupBar.SmallImages := Designer.GetRoot.FindComponent( S ) as TCustomImageList;
    DesignerModified;
  end;
end;


procedure TRzGroupBarEditor.LargeImagesMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    GroupBar.LargeImages := Designer.GetRoot.FindComponent( S ) as TCustomImageList;
    DesignerModified;
  end;
end;


procedure TRzGroupBarEditor.Edit;
begin
  // Do not add group on a double-click
end;



{============================}
{== TRzGroupEditor Methods ==}
{============================}

function TRzGroupEditor.Group: TRzGroup;
begin
  Result := Component as TRzGroup;
end;


function TRzGroupEditor.GetVerbCount: Integer;
begin
  Result := 16;
end;


function TRzGroupEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Add Item';
    1: Result := 'Assign ActionList';
    2: Result := '-';
    3: Result := 'Edit Items Collection';
    4: Result := 'Edit Item';
    5: Result := '-';
    6: Result := 'Item Style';
    7: Result := 'Special';
    8: Result := '-';
    9: Result := 'Set Group''s Small ImageList';
    10: Result := 'Set Group''s Large ImageList';
    11: Result := '-';
    12: Result := 'Add Group';
    13: Result := 'GroupBar Style';
    14: Result := 'Visual Style';
    15: Result := 'Gradient Color Style';
  end;
end;


function TRzGroupEditor.GetCompRefData( Index: Integer; var CompRefClass: TComponentClass;
                                        var CompRefPropName: string;
                                        var CompRefMenuHandler: TNotifyEvent ): Boolean;
begin
  Result := False;
  case Index of
    9:
    begin
      CompRefClass := TCustomImageList;
      CompRefPropName := 'SmallImages';
      CompRefMenuHandler := SmallImagesMenuHandler;
      Result := True;
    end;

    10:
    begin
      CompRefClass := TCustomImageList;
      CompRefPropName := 'LargeImages';
      CompRefMenuHandler := LargeImagesMenuHandler;
      Result := True;
    end;
  end;
end;


procedure TRzGroupEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
var
  I, K, ActionListCount: Integer;
  CompOwner: TComponent;
  ActList: TCustomActionList;
  CatList: TStringList;

  procedure CreateActionListMenu( ActionList: TCustomActionList; const Category: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    if Category <> '' then
      NewItem.Caption := ActionList.Name + '.' + Category
    else
      NewItem.Caption := ActionList.Name;
    NewItem.OnClick := ActionListMenuHandler;
    Item.Add( NewItem );
  end;

  procedure CreateActionListMenuSeparator;
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    NewItem.Caption := '-';
    Item.Add( NewItem );
  end;

  procedure CreateItemMenu( Index: Integer; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    if Caption = '-' then
      NewItem.Caption := 'Separator'
    else
      NewItem.Caption := Caption;
    NewItem.Tag := Index;
    NewItem.OnClick := ItemsMenuHandler;
    Item.Add( NewItem );
  end;


  procedure CreateItemStyleMenu( Style: TRzItemStyle; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Style );
    NewItem.Checked := Group.ItemStyle = Style;
    NewItem.OnClick := ItemStyleMenuHandler;
    Item.Add( NewItem );
  end;


  procedure CreateGroupBarStyleMenu( Style: TRzGroupBarStyle; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Style );
    NewItem.Checked := Group.GroupBar.Style = Style;
    NewItem.OnClick := GroupBarStyleMenuHandler;
    Item.Add( NewItem );
  end;


begin
  inherited;

  case Index of
    1: // Assign ActionList
    begin
      Item.AutoHotkeys := maManual;
      ActionListCount := 0;
      CompOwner := Designer.GetRoot;
      if CompOwner <> nil then
      begin
        for I := 0 to CompOwner.ComponentCount - 1 do
        begin
          if CompOwner.Components[ I ] is TCustomActionList then
          begin
            Inc( ActionListCount );
            ActList := TCustomActionList( CompOwner.Components[ I ] );

            // Create a menu item for All
            CreateActionListMenu( ActList, '' );

            // Create a menu items for each Category
            CatList := TStringList.Create;
            CatList.Sorted := True;
            CatList.Duplicates := dupIgnore;
            try
              for K := 0 to ActList.ActionCount - 1 do
              begin
                if ActList.Actions[ K ].Category <> '' then
                  CatList.Add( ActList.Actions[ K ].Category );
              end;

              for K := 0 to CatList.Count - 1 do
                CreateActionListMenu( ActList, CatList[ K ] );
            finally
              CatList.Free;
            end;

            if I < CompOwner.ComponentCount - 1 then
              CreateActionListMenuSeparator;
          end;
        end;
      end;
      Item.Enabled := ActionListCount > 0;
    end;

    4: // Edit Item
    begin
      Item.Enabled := Group.Items.Count > 0;
      for I := 0 to Group.Items.Count - 1 do
        CreateItemMenu( I, Group.Items[ I ].Caption );
    end;

    6: // Item Style
    begin
      CreateItemStyleMenu( isSmall, 'Small' );
      CreateItemStyleMenu( isLarge, 'Large' );
    end;

    7: Item.Checked := Group.Special;

    12:
    begin
      // Only allow user to add new groups if the group bar is NOT being edited in an inline frame.
      Item.Enabled := not IsInInlined;
    end;

    13: // GroupBar Style
    begin
      CreateGroupBarStyleMenu( gbsCategoryView, 'Category View' );
      CreateGroupBarStyleMenu( gbsTaskList, 'Task List' );
      CreateGroupBarStyleMenu( gbsOutlook, 'Outlook' );
    end;

    14: // Visual Style
    begin
      CreateVisualStyleMenuItem( Item, vsClassic, Group.GroupBar.VisualStyle,
                                 VisualStyleMenuHandler );
      CreateVisualStyleMenuItem( Item, vsWinXP, Group.GroupBar.VisualStyle,
                                 VisualStyleMenuHandler );
      CreateVisualStyleMenuItem( Item, vsGradient, Group.GroupBar.VisualStyle,
                                 VisualStyleMenuHandler );
    end;

    15: // Gradient Color Style
    begin
      CreateGradientColorStyleMenuItem( Item, gcsSystem, Group.GroupBar.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
      CreateGradientColorStyleMenuItem( Item, gcsMSOffice, Group.GroupBar.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
      CreateGradientColorStyleMenuItem( Item, gcsCustom, Group.GroupBar.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
    end;
  end;
end;


procedure TRzGroupEditor.ExecuteVerb( Index: Integer );
var
  G: TRzGroup;
begin
  case Index of
    0: // Add Item
    begin
      Group.Items.Add;
      DesignerModified;
      // Select the item we just added in the Object Inspector
      Designer.SelectComponent( Group.Items[ Group.Items.Count - 1 ] );
    end;

    3: // Edit Items Collection
    begin
      EditPropertyByName( 'Items' );
    end;

    7:
    begin
      Group.Special := not Group.Special;
      DesignerModified;
    end;

    12: // Add Group
    begin
      // Calling Designer.CreateComponent does not work b/c is causes the group to be first parented by the form
      G := Group.GroupBar.GroupClass.Create( Designer.GetRoot );
      G.Name := GetNewComponentName( Designer.GetRoot, 'RzGroup', False );
      Group.GroupBar.AddGroup( G );
      DesignerModified;
      Designer.SelectComponent( G );
    end;
  end;
end;


procedure TRzGroupEditor.ActionListMenuHandler( Sender: TObject );
var
  S, ActName, Category: string;
  ActionList: TCustomActionList;
  P: Integer;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;

    P := Pos( '.', S );
    if P > 0 then
    begin
      ActName := System.Copy( S, 1, P - 1 );
      Category := System.Copy( S, P + 1, 255 );
    end
    else
    begin
      ActName := S;
      Category := '';
    end;

    ActionList := Designer.GetRoot.FindComponent( ActName ) as TCustomActionList;
    Group.AssignActionList( ActionList, Category );
    DesignerModified;
  end;
end;


procedure TRzGroupEditor.ItemsMenuHandler( Sender: TObject );
begin
  Designer.SelectComponent( Group.Items[ TMenuItem( Sender ).Tag ] );
end;


procedure TRzGroupEditor.ItemStyleMenuHandler( Sender: TObject );
begin
  Group.ItemStyle := TRzItemStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzGroupEditor.SmallImagesMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    Group.SmallImages := Designer.GetRoot.FindComponent( S ) as TCustomImageList;
    DesignerModified;
  end;
end;


procedure TRzGroupEditor.LargeImagesMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    Group.LargeImages := Designer.GetRoot.FindComponent( S ) as TCustomImageList;
    DesignerModified;
  end;
end;


procedure TRzGroupEditor.GroupBarStyleMenuHandler( Sender: TObject );
begin
  Group.GroupBar.Style := TRzGroupBarStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzGroupEditor.VisualStyleMenuHandler( Sender: TObject );
begin
  Group.GroupBar.VisualStyle := TRzVisualStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzGroupEditor.GradientColorStyleMenuHandler( Sender: TObject );
begin
  Group.GroupBar.GradientColorStyle := TRzGradientColorStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzGroupEditor.Edit;
begin
  // Display the Collection Editor for the Items property
  EditPropertyByName( 'Items' );
end;




{======================================}
{== TRzGroupControllerEditor Methods ==}
{======================================}

function TRzGroupControllerEditor.GroupController: TRzGroupController;
begin
  Result := Component as TRzGroupController;
end;


function TRzGroupControllerEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;


function TRzGroupControllerEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Cateogry View Defaults';
    1: Result := 'Task List Defaults';
    2: Result := 'Outlook Defaults';
    3: Result := '-';
    4: Result := 'Set RegIniFile';
  end;
end;


function TRzGroupControllerEditor.GetCompRefData( Index: Integer; var CompRefClass: TComponentClass;
                                                  var CompRefPropName: string;
                                                  var CompRefMenuHandler: TNotifyEvent ): Boolean;
begin
  Result := False;
  if Index = 4 then
  begin
    CompRefClass := TRzRegIniFile;
    CompRefPropName := 'RegIniFile';
    CompRefMenuHandler := nil;
    Result := True;
  end
end;


procedure TRzGroupControllerEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    0:
    begin
      GroupController.SetDefaults( gbsCategoryView );
      DesignerModified;
    end;

    1: // Task List Defaults
    begin
      GroupController.SetDefaults( gbsTaskList );
      DesignerModified;
    end;

    2: // Outlook Defaults
    begin
      GroupController.SetDefaults( gbsOutlook );
      DesignerModified;
    end;
  end;
end;


procedure TRzGroupControllerEditor.Edit;
begin
  // Override with empty method b/c we do not want to invoke the first menu
  // item when the group controller is double-clicked.
end;



{====================================}
{== TRzGroupTemplateEditor Methods ==}
{====================================}

function TRzGroupTemplateEditor.Template: TRzGroupTemplate;
begin
  Result := Component as TRzGroupTemplate;
end;


function TRzGroupTemplateEditor.GetVerbCount: Integer;
begin
  Result := 10;
end;


function TRzGroupTemplateEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Edit Items Collection';
    1: Result := 'Preview Group...';
    2: Result := '-';
    3: Result := 'Assign ActionList';
    4: Result := '-';
    5: Result := 'Special';
    6: Result := 'Can Close';
    7: Result := '-';
    8: Result := 'Set Template''s Small ImageList';
    9: Result := 'Set Template''s Large ImageList';
  end;
end;


procedure TRzGroupTemplateEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
var
  I, K, ActionListCount, ImageListCount: Integer;
  CompOwner: TComponent;
  ActList: TCustomActionList;
  CatList: TStringList;

  procedure CreateActionListMenu( ActionList: TCustomActionList; const Category: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    if Category <> '' then
      NewItem.Caption := ActionList.Name + '.' + Category
    else
      NewItem.Caption := ActionList.Name;
    NewItem.OnClick := ActionListMenuHandler;
    Item.Add( NewItem );
  end;

  procedure CreateActionListMenuSeparator;
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    NewItem.Caption := '-';
    Item.Add( NewItem );
  end;


  procedure CreateLargeImageListMenu( ImageList: TCustomImageList );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    NewItem.Caption := ImageList.Name;
    NewItem.Checked := Template.PreviewOptions.LargeImages = ImageList;
    NewItem.OnClick := LargeImagesMenuHandler;
    Item.Add( NewItem );
  end;


  procedure CreateSmallImageListMenu( ImageList: TCustomImageList );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    NewItem.Caption := ImageList.Name;
    NewItem.Checked := Template.PreviewOptions.SmallImages = ImageList;
    NewItem.OnClick := SmallImagesMenuHandler;
    Item.Add( NewItem );
  end;


begin {= TRzGroupTemplateEditor.PrepareMenuItem =}
  inherited;

  case Index of
    3: // Assign ActionList
    begin
      Item.AutoHotkeys := maManual;
      ActionListCount := 0;
      CompOwner := Designer.GetRoot;
      if CompOwner <> nil then
      begin
        for I := 0 to CompOwner.ComponentCount - 1 do
        begin
          if CompOwner.Components[ I ] is TCustomActionList then
          begin
            Inc( ActionListCount );
            ActList := TCustomActionList( CompOwner.Components[ I ] );

            // Create a menu item for All
            CreateActionListMenu( ActList, '' );

            // Create a menu items for each Category
            CatList := TStringList.Create;
            CatList.Sorted := True;
            CatList.Duplicates := dupIgnore;
            try
              for K := 0 to ActList.ActionCount - 1 do
              begin
                if ActList.Actions[ K ].Category <> '' then
                  CatList.Add( ActList.Actions[ K ].Category );
              end;

              for K := 0 to CatList.Count - 1 do
                CreateActionListMenu( ActList, CatList[ K ] );
            finally
              CatList.Free;
            end;

            if I < CompOwner.ComponentCount - 1 then
              CreateActionListMenuSeparator;
          end;
        end;
      end;
      Item.Enabled := ActionListCount > 0;
    end;

    5: Item.Checked := Template.Special;
    6: Item.Checked := Template.CanClose;


    8, 9: // Image Lists
    begin
      Item.AutoHotkeys := maManual;
      ImageListCount := 0;
      CompOwner := Designer.GetRoot;

      if CompOwner <> nil then
      begin
        for I := 0 to CompOwner.ComponentCount - 1 do
        begin
          if CompOwner.Components[ I ] is TCustomImageList then
          begin
            Inc( ImageListCount );
            if Index = 8 then
              CreateSmallImageListMenu( TCustomImageList( CompOwner.Components[ I ] ) )
            else
              CreateLargeImageListMenu( TCustomImageList( CompOwner.Components[ I ] ) );
          end;
        end;
      end;
      Item.Enabled := ImageListCount > 0;
    end;

  end;
end; {= TRzGroupTemplateEditor.PrepareMenuItem =}


procedure TRzGroupTemplateEditor.ExecuteVerb( Index: Integer );
var
  Dlg: TRzGroupTemplatePreviewDlg;
begin
  case Index of
    0: // Edit Items Collection
    begin
      EditPropertyByName( 'Items' );
    end;

    1: // Preview Group
    begin
      Dlg := TRzGroupTemplatePreviewDlg.Create( Application );

      try
        Dlg.SetTemplate( Template );
        Dlg.ShowModal;
      finally
        Dlg.Free;
      end;
    end;

    5:
    begin
      Template.Special := not Template.Special;
      DesignerModified;
    end;

    6:
    begin
      Template.CanClose := not Template.CanClose;
      DesignerModified;
    end;
  end;
end;


procedure TRzGroupTemplateEditor.ActionListMenuHandler( Sender: TObject );
var
  S, ActName, Category: string;
  ActionList: TCustomActionList;
  P: Integer;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;

    P := Pos( '.', S );
    if P > 0 then
    begin
      ActName := System.Copy( S, 1, P - 1 );
      Category := System.Copy( S, P + 1, 255 );
    end
    else
    begin
      ActName := S;
      Category := '';
    end;

    ActionList := Designer.GetRoot.FindComponent( ActName ) as TCustomActionList;
    Template.AssignActionList( ActionList, Category );
    DesignerModified;
  end;
end;


procedure TRzGroupTemplateEditor.SmallImagesMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    Template.PreviewOptions.SmallImages := Designer.GetRoot.FindComponent( S ) as TCustomImageList;
    DesignerModified;
  end;
end;


procedure TRzGroupTemplateEditor.LargeImagesMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    Template.PreviewOptions.LargeImages := Designer.GetRoot.FindComponent( S ) as TCustomImageList;
    DesignerModified;
  end;
end;


procedure TRzGroupTemplateEditor.Edit;
begin
  // Display the Collection Editor for the Items property
  EditPropertyByName( 'Items' );
end;


{========================================}
{== TRzGroupTemplatePreviewDlg Methods ==}
{========================================}

procedure TRzGroupTemplatePreviewDlg.FormCreate(Sender: TObject);
begin
  Icon.Handle := LoadIcon( HInstance, 'RZDESIGNEDITORS_PREVIEW_GROUPTEMPLATE_ICON' );
end;


procedure TRzGroupTemplatePreviewDlg.SetTemplate( Template: TRzGroupTemplate );
var
  I, H: Integer;
  G: TRzGroup;
  Item: TRzGroupItem;
begin
  Caption := Template.Name;

  G := TRzGroup.Create( GroupBar );
  GroupBar.AddGroup( G );

  G.CanClose := Template.CanClose;
  G.Caption := Template.Caption;
  G.CaptionImageIndex := Template.CaptionImageIndex;
  G.Opened := Template.Opened;
  G.Special := Template.Special;

  // Create items from Template
  for I := 0 to Template.Items.Count - 1 do
  begin
    Item := G.Items.Add;
    Item.Caption := Template.Items[ I ].Caption;
    Item.DisabledIndex := Template.Items[ I ].DisabledIndex;
    Item.Enabled := Template.Items[ I ].Enabled;
    Item.Hint := Template.Items[ I ].Hint;
    Item.ImageIndex := Template.Items[ I ].ImageIndex;
    Item.IndentLevel := Template.Items[ I ].IndentLevel;
    Item.Tag := Template.Items[ I ].Tag;
    Item.Visible := Template.Items[ I ].Visible;
    if ( Assigned( Template.Items[ I ].OnClick ) or ( Template.Items[ I ].Action <> nil ) ) then
      Item.OnClick := ItemClickHandler;
  end;

  GroupBar.SmallImages := Template.PreviewOptions.SmallImages;
  GroupBar.LargeImages := Template.PreviewOptions.LargeImages;
  G.ItemStyle := Template.PreviewOptions.ItemStyle;

  H := GroupBar.Groups[ 0 ].CalculateHeight( GroupBar.ClientWidth ) + 20;
  if H < 92 then
    H := 92;
  if H > Screen.Height then
    H := Screen.Height;
  ClientHeight := H;
end;


procedure TRzGroupTemplatePreviewDlg.FormKeyPress( Sender: TObject; var Key: Char );
begin
  if Key = #27 then
    ModalResult := mrOK;
end;


procedure TRzGroupTemplatePreviewDlg.ItemClickHandler( Sender: TObject );
begin
end;



{==============================}
{== TRzGroupDesigner Methods ==}
{==============================}

{$IFDEF USE_GROUP_DESIGNER}

constructor TRzGroupDesigner.Create( AOwner: TComponent );
begin
  inherited;
  RegisterDesignNotification( Self );
end;


destructor TRzGroupDesigner.Destroy;
begin
  UnregisterDesignNotification( Self );
  inherited;
end;


procedure TRzGroupDesigner.DesignerOpened( const ADesigner: IDesigner; AResurrecting: Boolean );
begin
  FDesigner := ADesigner;
end;


procedure TRzGroupDesigner.DesignerClosed( const ADesigner: IDesigner; AGoingDormant: Boolean );
begin
  if FDesigner = ADesigner then
    FDesigner := nil;
end;


procedure TRzGroupDesigner.ItemDeleted( const ADesigner: IDesigner; AItem: TPersistent );
begin
end;

procedure TRzGroupDesigner.ItemInserted( const ADesigner: IDesigner; AItem: TPersistent );
begin
end;

procedure TRzGroupDesigner.ItemsModified( const ADesigner: IDesigner );
begin
end;

procedure TRzGroupDesigner.SelectionChanged( const ADesigner: IDesigner; const ASelection: IDesignerSelections );
begin
end;


procedure TRzGroupDesigner.SetSelection( APersistent: TPersistent );
var
  List: IDesignerSelections;
begin
  if Assigned( FDesigner ) then
  begin
    List := CreateSelectionList;
    List.Add( APersistent );
    FDesigner.SetSelections( List );
  end;
end;

{$ENDIF}

{$IFDEF USE_GROUP_DESIGNER}

var
  GroupDesignerObj: TRzGroupDesigner;

procedure Register;
begin
  GroupDesignerObj := TRzGroupDesigner.Create( nil );
  // Assign Interface reference used by the TRzGroupBar component
  GroupDesigner := GroupDesignerObj;
end;


initialization

finalization
  GroupDesigner := nil;
  GroupDesignerObj.Free;

{$ENDIF}

end.
