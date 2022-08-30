{===============================================================================
  RzToolbarEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzToolbarEditor
    Adds context menu and advanced editing dialog.


  Modification History
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed positioning of Raize Toolbar Editor so that the design editor
      appears just under the toolbar being edited if possible.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Rearranged and incorporated brand new 256 color images for glyphs.
    * Redesigned the Custom Image area to mimic the stock image area.
    * Added the Images list, which displays all images currently in the
      associated ImageList.
    * The editor now supports specifying a suffix to be used when creating the
      name for a newly created button.
===============================================================================}

{$I RzComps.inc}

unit RzToolbarEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  DesignIntf,
  DesignEditors,
  DesignMenus,
  Forms,
  Dialogs,
  Buttons,
  StdCtrls,
  RzPanel,
  RzLabel,
  ExtCtrls,
  Menus,
  RzSpnEdt,
  RzBorder,
  RzRadGrp,
  Mask,
  RzEdit,
  RzButton,
  RzDesignEditors,
  RzLstBox,
  RzRadChk,
  ExtDlgs;


type
  TRzToolbarEditor = class( TRzComponentEditor )
  protected
    function Toolbar: TRzToolbar;
    function GetCompRefData( Index: Integer; var CompRefClass: TComponentClass; var CompRefPropName: string;
                             var CompRefMenuHandler: TNotifyEvent ): Boolean; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure LayoutMenuHandler( Sender: TObject );
    procedure SettingsMenuHandler( Sender: TObject );
    procedure VisualStyleMenuHandler( Sender: TObject );
    procedure GradientColorStyleMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ): string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  TRzToolbarEditDlg = class(TForm)
    MnuStock: TPopupMenu;
    MnuEditStockHint: TMenuItem;
    GrpControls: TRzGroupBox;
    LstOrder: TRzListBox;
    Timer1: TTimer;
    RzPanel1: TRzPanel;
    BtnMoveUp: TRzToolbarButton;
    BtnMoveDown: TRzToolbarButton;
    BtnDeleteControl: TRzToolbarButton;
    BtnChangePrefix: TSpeedButton;
    GrpStockImages: TRzGroupBox;
    BtnSpacer: TSpeedButton;
    BtnGrooveSpacer: TSpeedButton;
    RzBorder1: TRzBorder;
    GrpCustomImages: TRzGroupBox;
    SbxCustom: TScrollBox;
    MnuCustomImages: TPopupMenu;
    AddCustomImage1: TMenuItem;
    DlgOpenPicture: TOpenPictureDialog;
    LblNoImageListMsg: TRzLabel;
    LblNoImageListTitle: TRzLabel;
    BtnNew: TRzToolbarButton;
    BtnOpen: TRzToolbarButton;
    BtnSave: TRzToolbarButton;
    BtnFolderClosed: TRzToolbarButton;
    BtnFolderSelect: TRzToolbarButton;
    BtnCut: TRzToolbarButton;
    BtnCopy: TRzToolbarButton;
    BtnPaste: TRzToolbarButton;
    BtnUndo: TRzToolbarButton;
    BtnRedo: TRzToolbarButton;
    BtnSearchFind: TRzToolbarButton;
    BtnSearchReplace: TRzToolbarButton;
    BtnSearchFindNext: TRzToolbarButton;
    BtnFormatBold: TRzToolbarButton;
    BtnFormatItalics: TRzToolbarButton;
    BtnFormatUnderline: TRzToolbarButton;
    BtnFormatFont: TRzToolbarButton;
    BtnFormatLeft: TRzToolbarButton;
    BtnFormatCenter: TRzToolbarButton;
    BtnFormatRight: TRzToolbarButton;
    BtnFormatJustify: TRzToolbarButton;
    BtnFormatBullets: TRzToolbarButton;
    BtnFolderUp: TRzToolbarButton;
    BtnFolderNew: TRzToolbarButton;
    BtnHelp: TRzToolbarButton;
    BtnFolderOpen: TRzToolbarButton;
    BtnSearchJumpToLine: TRzToolbarButton;
    BtnDBPrevious: TRzToolbarButton;
    BtnDBNext: TRzToolbarButton;
    BtnDBLast: TRzToolbarButton;
    BtnDBEdit: TRzToolbarButton;
    BtnDBPost: TRzToolbarButton;
    BtnDBCancel: TRzToolbarButton;
    BtnDBRefresh: TRzToolbarButton;
    BtnOK: TRzToolbarButton;
    BtnCancel: TRzToolbarButton;
    BtnSignalError: TRzToolbarButton;
    BtnHelpBook: TRzToolbarButton;
    BtnHelpContext: TRzToolbarButton;
    BtnEmail: TRzToolbarButton;
    BtnAttach: TRzToolbarButton;
    BtnWindowCascade: TRzToolbarButton;
    BtnWindowHorzTile: TRzToolbarButton;
    BtnWindowVertTile: TRzToolbarButton;
    BtnWindowTile: TRzToolbarButton;
    BtnViewZoom: TRzToolbarButton;
    BtnViewZoomOut: TRzToolbarButton;
    BtnViewZoomIn: TRzToolbarButton;
    BtnToolsCursor: TRzToolbarButton;
    BtnRecycle: TRzToolbarButton;
    BtnRecycleXP: TRzToolbarButton;
    BtnClear: TRzToolbarButton;
    BtnToolsImage: TRzToolbarButton;
    BtnPrint: TRzToolbarButton;
    BtnPrintPreview: TRzToolbarButton;
    BtnCalendarDate: TRzToolbarButton;
    BtnProperties: TRzToolbarButton;
    BtnExecute: TRzToolbarButton;
    BtnToolsRuler: TRzToolbarButton;
    BtnToolsHammer: TRzToolbarButton;
    BtnExit: TRzToolbarButton;
    BtnSignalFinish: TRzToolbarButton;
    BtnArrowLeft: TRzToolbarButton;
    BtnArrowUp: TRzToolbarButton;
    BtnArrowRight: TRzToolbarButton;
    BtnArrowDown: TRzToolbarButton;
    BtnMoveLeft: TRzToolbarButton;
    BtnMoveAllLeft: TRzToolbarButton;
    BtnMoveRight: TRzToolbarButton;
    BtnMoveAllRight: TRzToolbarButton;
    BtnCalendarMonth: TRzToolbarButton;
    BtnMove: TRzToolbarButton;
    BtnCopyAll: TRzToolbarButton;
    BtnSaveAll: TRzToolbarButton;
    BtnNotebook: TRzToolbarButton;
    BtnNote: TRzToolbarButton;
    BtnNotePage: TRzToolbarButton;
    BtnImport: TRzToolbarButton;
    BtnExport: TRzToolbarButton;
    BtnToolsPencil: TRzToolbarButton;
    BtnToolsPen: TRzToolbarButton;
    BtnToolsKey: TRzToolbarButton;
    BtnDBFirst: TRzToolbarButton;
    BtnPreviewNext: TRzToolbarButton;
    BtnPreviewPrev: TRzToolbarButton;
    BtnAlign: TRzToolbarButton;
    BtnAlignLeft: TRzToolbarButton;
    BtnAlignTop: TRzToolbarButton;
    BtnAlignRight: TRzToolbarButton;
    BtnAlignBottom: TRzToolbarButton;
    BtnAlignClient: TRzToolbarButton;
    BtnFileDelete: TRzToolbarButton;
    BtnAlignNone: TRzToolbarButton;
    BtnNetWeb: TRzToolbarButton;
    BtnNetNews: TRzToolbarButton;
    BtnOrderBackOne: TRzToolbarButton;
    BtnOrderFrontOne: TRzToolbarButton;
    BtnOrderToBack: TRzToolbarButton;
    BtnOrderToFront: TRzToolbarButton;
    BtnMediaSkipBackward: TRzToolbarButton;
    BtnMediaRewind: TRzToolbarButton;
    BtnMediaStop: TRzToolbarButton;
    BtnMediaPause: TRzToolbarButton;
    BtnMediaPlay: TRzToolbarButton;
    BtnMediaFastForward: TRzToolbarButton;
    BtnMediaSkipForward: TRzToolbarButton;
    BtnMediaRecord: TRzToolbarButton;
    BtnSelectAll: TRzToolbarButton;
    BtnToolsPlug: TRzToolbarButton;
    BtnOptions: TRzToolbarButton;
    BtnToolsSpeaker: TRzToolbarButton;
    BtnToolsPin: TRzToolbarButton;
    BtnViewIcons: TRzToolbarButton;
    BtnCalendarWeek: TRzToolbarButton;
    BtnSignalRed: TRzToolbarButton;
    BtnSignalOrange: TRzToolbarButton;
    BtnSignalGreen: TRzToolbarButton;
    BtnSignalLtBlue: TRzToolbarButton;
    BtnSignalYellow: TRzToolbarButton;
    BtnSignalReminder: TRzToolbarButton;
    BtnSignalWarning: TRzToolbarButton;
    BtnSignalBlue: TRzToolbarButton;
    BtnFormatWordWrap: TRzToolbarButton;
    BtnSignalGray: TRzToolbarButton;
    BtnFormatTabs: TRzToolbarButton;
    BtnSignalInfo: TRzToolbarButton;
    BtnSignalViolet: TRzToolbarButton;
    BtnViewList: TRzToolbarButton;
    BtnViewDetails: TRzToolbarButton;
    BtnDBInsert: TRzToolbarButton;
    BtnDBDelete: TRzToolbarButton;
    PnlStyle: TRzPanel;
    PnlClose: TRzPanel;
    GrpStyle: TRzGroupBox;
    BtnNoCaptionsStyle: TRzToolbarButton;
    BtnCaptionsBottomStyle: TRzToolbarButton;
    BtnCaptionsRightStyle: TRzToolbarButton;
    ChkAddDisabled: TRzCheckBox;
    BtnDone: TRzButton;
    MnuCustom: TPopupMenu;
    MnuAdd: TMenuItem;
    MnuSep1: TMenuItem;
    MnuEditCustomHint: TMenuItem;
    MnuDelete: TMenuItem;
    MnuControls: TPopupMenu;
    MnuDeleteControl: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ToolbarButtonClick(Sender: TObject);
    procedure BtnSpacerClick(Sender: TObject);
    procedure MnuAddClick(Sender: TObject);
    procedure MnuDeleteClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MnuEditStockHintClick(Sender: TObject);
    procedure BtnMoveDownClick(Sender: TObject);
    procedure BtnMoveUpClick(Sender: TObject);
    procedure LstOrderClick(Sender: TObject);
    procedure LstOrderDragDrop( Sender, Source: TObject; X, Y: Integer);
    procedure LstOrderDragOver( Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean );
    procedure LstOrderDrawItem( Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState );
    procedure LstOrderEndDrag( Sender, Target: TObject; X, Y: Integer );
    procedure LstOrderMouseDown( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
    procedure Timer1Timer(Sender: TObject);
    procedure BtnDeleteControlClick(Sender: TObject);
    procedure BtnChangePrefixClick(Sender: TObject);
    procedure ButtonStyleClick(Sender: TObject);
    procedure MnuEditCustomHintClick(Sender: TObject);
    procedure SbxCustomResize(Sender: TObject);
    procedure MnuPrefixClick(Sender: TObject);
    procedure MnuSuffixClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FUsePrefix: Boolean;
    FPrefix: string;
    OldIdx, NewIdx: Integer;
    GoingUp: Boolean;

    FCustomImages: TStringList;
    procedure RearrangeCustomButtons;
    function CreateCustomButton( const S: string ): TRzToolbarButton;
    function GetFileNameFromString( const S: string ): string;
    function GetHintFromString( const S: string ): string;

    procedure EnableMoveButtons( Idx: Integer );
    function CreateNewButton( const Hint: string ): TRzToolButton;
    procedure AddImageToImageList( Btn: TRzToolButton; Glyph: TBitmap );
  public
    SelectedBtn: TRzToolbarButton;
    CompOwner: TComponent;
    Toolbar: TRzToolbar;
    procedure UpdateControls;
    procedure Reposition;
  end;


implementation

{$R *.dfm}

uses
  RzCommon,
  IniFiles,
  Registry,
  ImgList,
  TypInfo,
  RzToolbarPrefixForm;

const
  CustomButtonsSection = 'CustomButtons';
  StockHintsSection = 'StockHints';
  ToolbarButtonsSection = 'ToolbarButtons';
  ToolbarEditorSection = 'ToolbarEditor';


{==============================}
{== TRzToolbarEditor Methods ==}
{==============================}

function TRzToolbarEditor.Toolbar: TRzToolbar;
begin
  Result := Component as TRzToolbar;
end;


function TRzToolbarEditor.GetVerbCount: Integer;
begin
  Result := 12;
end;


function TRzToolbarEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Edit Toolbar...';
    1: Result := 'Add Tool Button';
    2: Result := 'Add Spacer';
    3: Result := '-';
    4: Result := 'Set Image List';
    5: Result := '-';
    6: Result := 'Visual Style';
    7: Result := 'Gradient Color Style';
    8: Result := 'Image Layout';

    9:
    begin
      if Toolbar.ShowDivider then
        Result := 'Hide Divider'
      else
        Result := 'Show Divider';
    end;

    10: Result := 'Quick Settings';
    11: Result := 'Set RegIniFile';
  end;
end;



function TRzToolbarEditor.GetCompRefData( Index: Integer; var CompRefClass: TComponentClass;
                                          var CompRefPropName: string; var CompRefMenuHandler: TNotifyEvent ): Boolean;
begin
  Result := False;
  if Index = 4 then
  begin
    CompRefClass := TCustomImageList;
    CompRefPropName := 'Images';
    CompRefMenuHandler := nil;
    Result := True;
  end
  else if Index = 11 then
  begin
    CompRefClass := TRzRegIniFile;
    CompRefPropName := 'RegIniFile';
    CompRefMenuHandler := nil;
    Result := True;
  end;
end;


procedure TRzToolbarEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );

  procedure CreateLayoutMenu( Layout: Integer; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Layout );
    NewItem.OnClick := LayoutMenuHandler;
    Item.Add( NewItem );
  end;


  procedure CreateSettingsMenu( Settings: Integer; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Settings );
    NewItem.OnClick := SettingsMenuHandler;
    Item.Add( NewItem );
  end;


begin
  inherited;

  case Index of
    0:
    begin
      // Only allow user to add new buttons if the toolbar is NOT being edited in an inline frame (i.e. a frame instance).
      Item.Enabled := not IsInInlined;
    end;

    6: // VisualStyle
    begin
      CreateVisualStyleMenuItem( Item, vsClassic, Toolbar.VisualStyle,
                                 VisualStyleMenuHandler );
      CreateVisualStyleMenuItem( Item, vsWinXP, Toolbar.VisualStyle,
                                 VisualStyleMenuHandler );
      CreateVisualStyleMenuItem( Item, vsGradient, Toolbar.VisualStyle,
                                 VisualStyleMenuHandler );
    end;

    7: // GradientColorStyle
    begin
      CreateGradientColorStyleMenuItem( Item, gcsSystem, Toolbar.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
      CreateGradientColorStyleMenuItem( Item, gcsMSOffice, Toolbar.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
      CreateGradientColorStyleMenuItem( Item, gcsCustom, Toolbar.GradientColorStyle,
                                        GradientColorStyleMenuHandler );
    end;

    8: // Image Layout
    begin
      CreateLayoutMenu( 0, 'No Captions' );
      CreateLayoutMenu( 1, 'Captions on Bottom' );
      CreateLayoutMenu( 2, 'Captions on Right' );
    end;

    10: // Quick Settings
    begin
      CreateSettingsMenu( 0, 'Main Container' );
      CreateSettingsMenu( 1, 'Nested Toolbar' );
      CreateSettingsMenu( 2, 'For use in a ControlBar' );
    end;
  end;
end;


procedure TRzToolbarEditor.ExecuteVerb( Index: Integer );
var
  Dlg: TRzToolbarEditDlg;
  OwnerName: string;
  Spacer: TRzSpacer;
  Button: TRzToolButton;
begin
  case Index of
    0: // Edit Toolbar...
    begin
      Dlg := TRzToolbarEditDlg.Create( Application );
      try
        if Component.Owner <> nil then
          OwnerName := Component.Owner.Name + '.'
        else
          OwnerName := '';
        Dlg.Caption := OwnerName + Component.Name + Dlg.Caption;

        Dlg.CompOwner := Designer.GetRoot;

        Dlg.Toolbar := TRzToolbar( Component );
        Dlg.Reposition;
        Dlg.UpdateControls;
        Dlg.ShowModal;
        DesignerModified;
      finally
        Dlg.Free;
      end;
    end;


    1: // Add Tool Button
    begin
      if Designer.GetRoot <> nil then
      begin
        Button := TRzToolButton.Create( Designer.GetRoot );
        Button.Parent := Toolbar;
        Button.Name := GetNewComponentName( Designer.GetRoot, 'RzToolButton', False );
        DesignerModified;
      end;
    end;

    2: // Add Spacer
    begin
      if Designer.GetRoot <> nil then
      begin
        Spacer := TRzSpacer.Create( Designer.GetRoot );
        Spacer.Parent := Toolbar;
        Spacer.Name := GetNewComponentName( Designer.GetRoot, 'RzSpacer', False );
        DesignerModified;
      end;
    end;

    9: // Show/Hide Divider
    begin
      Toolbar.ShowDivider := not Toolbar.ShowDivider;
      DesignerModified;
    end;

  end; { case }
end; {= TRzToolbarEditor.ExecuteVerb =}


procedure TRzToolbarEditor.LayoutMenuHandler( Sender: TObject );
var
  W, H: Integer;
  ShowCaptions: Boolean;
  Layout: TButtonLayout;
begin
  case TMenuItem( Sender ).Tag of
    0:
    begin
      W := 25;
      H := 25;
      ShowCaptions := False;
      Layout := blGlyphLeft;
    end;

    1:
    begin
      W := 60;
      H := 40;
      ShowCaptions := True;
      Layout := blGlyphTop;
    end;

    else
    begin
      W := 60;
      H := 25;
      ShowCaptions := True;
      Layout := blGlyphLeft;
    end;
  end;

  Toolbar.ButtonLayout := Layout;
  Toolbar.UpdateButtonSize( W, H, ShowCaptions );

  if Toolbar.Orientation = orHorizontal then
    Toolbar.RowHeight := H
  else
    Toolbar.RowHeight := W;
  DesignerModified;
end;


procedure TRzToolbarEditor.SettingsMenuHandler( Sender: TObject );
begin
  case TMenuItem( Sender ).Tag of
    0: // Container Toolbar
    begin
      Toolbar.AutoStyle := False;
      Toolbar.Margin := 1;
      Toolbar.TopMargin := 1;
      Toolbar.RowHeight := 29;
      Toolbar.BorderInner := fsNone;
      Toolbar.BorderOuter := fsStatus;
      Toolbar.BorderSides := sdAllSides;
    end;

    1: // Nested Toolbar
    begin
      Toolbar.AutoStyle := False;
      Toolbar.BorderOuter := fsPopup;
      Toolbar.BorderSides := sdAllSides;
    end;

    2: // ControlBar Settings
    begin
      Toolbar.RowHeight := 22;
      Toolbar.Margin := 0;
      Toolbar.TopMargin := 0;
      Toolbar.AutoResize := False;
      Toolbar.ShowDivider := False;
      Toolbar.WrapControls := False;
    end;
  end;
  DesignerModified;
end;


procedure TRzToolbarEditor.VisualStyleMenuHandler( Sender: TObject );
begin
  Toolbar.VisualStyle := TRzVisualStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzToolbarEditor.GradientColorStyleMenuHandler( Sender: TObject );
begin
  Toolbar.GradientColorStyle := TRzGradientColorStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;



{===============================}
{== TRzToolbarEditDlg Methods ==}
{===============================}

procedure TRzToolbarEditDlg.FormCreate(Sender: TObject);
var
  BtnCount: Integer;
  I: Integer;
  S: string;
  R: TRegIniFile;
begin
  PopupMode := pmAuto;
  Position := poDesigned;

  Icon.Handle := LoadIcon( HInstance, 'RZDESIGNEDITORS_EDIT_TOOLBAR_ICON' );

  FCustomImages := TStringList.Create;

  R := TRegIniFile.Create( RC_SettingsKey );
  try
    // Read in stock hints
    BtnNew.Hint := R.ReadString( StockHintsSection, 'New', 'New' );
    BtnOpen.Hint := R.ReadString( StockHintsSection, 'Open', 'Open' );
    BtnSave.Hint := R.ReadString( StockHintsSection, 'Save', 'Save' );
    BtnSaveAll.Hint := R.ReadString( StockHintsSection, 'SaveAll', 'Save All' );
    BtnFileDelete.Hint := R.ReadString( StockHintsSection, 'Delete', 'Delete' );
    BtnPrint.Hint := R.ReadString( StockHintsSection, 'Print', 'Print' );
    BtnPrintPreview.Hint := R.ReadString( StockHintsSection, 'PrintPreview', 'Print Preview' );
    BtnPreviewPrev.Hint := R.ReadString( StockHintsSection, 'PreviewPreviousPage', 'Preview Previous Page' );
    BtnPreviewNext.Hint := R.ReadString( StockHintsSection, 'PreviewNextPage', 'Preview Next Page' );
    BtnImport.Hint := R.ReadString( StockHintsSection, 'Import', 'Import' );
    BtnExport.Hint := R.ReadString( StockHintsSection, 'Export', 'Export' );
    BtnFolderOpen.Hint := R.ReadString( StockHintsSection, 'Open', 'Open' );
    BtnFolderClosed.Hint := R.ReadString( StockHintsSection, 'Close', 'Close' );
    BtnFolderUp.Hint := R.ReadString( StockHintsSection, 'UpOneLevel', 'Up One Level' );
    BtnFolderNew.Hint := R.ReadString( StockHintsSection, 'NewFolder', 'New Folder' );
    BtnFolderSelect.Hint := R.ReadString( StockHintsSection, 'SelectFolder', 'Select Folder' );
    BtnExit.Hint := R.ReadString( StockHintsSection, 'Exit', 'Exit' );
    BtnCut.Hint := R.ReadString( StockHintsSection, 'Cut', 'Cut' );
    BtnCopy.Hint := R.ReadString( StockHintsSection, 'Copy', 'Copy' );
    BtnPaste.Hint := R.ReadString( StockHintsSection, 'Paste', 'Paste' );
    BtnUndo.Hint := R.ReadString( StockHintsSection, 'Undo', 'Undo' );
    BtnRedo.Hint := R.ReadString( StockHintsSection, 'Redo', 'Redo' );
    BtnSelectAll.Hint := R.ReadString( StockHintsSection, 'SelectAll', 'Select All' );
    BtnClear.Hint := R.ReadString( StockHintsSection, 'Clear', 'Clear' );
    BtnRecycle.Hint := R.ReadString( StockHintsSection, 'Recycle', 'Recycle' );
    BtnRecycleXP.Hint := R.ReadString( StockHintsSection, 'Recycle', 'Recycle' );
    BtnCopyAll.Hint := R.ReadString( StockHintsSection, 'CopyAll', 'Copy All' );
    BtnMove.Hint := R.ReadString( StockHintsSection, 'Move', 'Move' );
    BtnProperties.Hint := R.ReadString( StockHintsSection, 'Properties', 'Properties' );
    BtnOptions.Hint := R.ReadString( StockHintsSection, 'ChangeOptions', 'Change Options' );
    BtnCalendarDate.Hint := R.ReadString( StockHintsSection, 'DateView', 'Date View' );
    BtnCalendarWeek.Hint := R.ReadString( StockHintsSection, 'WeekView', 'Week View' );
    BtnCalendarMonth.Hint := R.ReadString( StockHintsSection, 'MonthView', 'Month View' );
    BtnSearchFind.Hint := R.ReadString( StockHintsSection, 'Find', 'Find' );
    BtnSearchReplace.Hint := R.ReadString( StockHintsSection, 'Replace', 'Replace' );
    BtnSearchFindNext.Hint := R.ReadString( StockHintsSection, 'FindNext', 'Find Next' );
    BtnSearchJumpToLine.Hint := R.ReadString( StockHintsSection, 'JumpToLine', 'Jump to Line' );
    BtnDBFirst.Hint := R.ReadString( StockHintsSection, 'FirstRecord', 'First Record' );
    BtnDBPrevious.Hint := R.ReadString( StockHintsSection, 'PreviousRecord', 'Previous Record' );
    BtnDBNext.Hint := R.ReadString( StockHintsSection, 'NextRecord', 'Next Record' );
    BtnDBLast.Hint := R.ReadString( StockHintsSection, 'LastRecord', 'Last Record' );
    BtnDBInsert.Hint := R.ReadString( StockHintsSection, 'InsertRecord', 'Insert Record' );
    BtnDBDelete.Hint := R.ReadString( StockHintsSection, 'DeleteRecord', 'Delete Record' );
    BtnDBEdit.Hint := R.ReadString( StockHintsSection, 'EditRecord', 'Edit Record' );
    BtnDBPost.Hint := R.ReadString( StockHintsSection, 'PostRecord', 'Post' );
    BtnDBCancel.Hint := R.ReadString( StockHintsSection, 'CancelChanges', 'Cancel' );
    BtnDBRefresh.Hint := R.ReadString( StockHintsSection, 'Refresh', 'Refresh' );
    BtnHelp.Hint := R.ReadString( StockHintsSection, 'Help', 'Help' );
    BtnHelpBook.Hint := R.ReadString( StockHintsSection, 'HelpBook', 'Help' );
    BtnHelpContext.Hint := R.ReadString( StockHintsSection, 'ContextHelp', 'Context Help' );
    BtnNetWeb.Hint := R.ReadString( StockHintsSection, 'Internet', 'Internet' );
    BtnNetNews.Hint := R.ReadString( StockHintsSection, 'Newsgroups', 'Newsgroups' );
    BtnWindowCascade.Hint := R.ReadString( StockHintsSection, 'Cascade', 'Cascade' );
    BtnWindowHorzTile.Hint := R.ReadString( StockHintsSection, 'HorizTile', 'Horizontal Tile' );
    BtnWindowVertTile.Hint := R.ReadString( StockHintsSection, 'VertTile', 'Vertical Tile' );
    BtnWindowTile.Hint := R.ReadString( StockHintsSection, 'Tile', 'Tile' );
    BtnViewIcons.Hint := R.ReadString( StockHintsSection, 'ViewIcons', 'View Icons' );
    BtnViewList.Hint := R.ReadString( StockHintsSection, 'ViewList', 'View List' );
    BtnViewDetails.Hint := R.ReadString( StockHintsSection, 'ViewDetails', 'View Details' );
    BtnViewZoom.Hint := R.ReadString( StockHintsSection, 'View', 'View' );
    BtnViewZoomOut.Hint := R.ReadString( StockHintsSection, 'ZoomOut', 'Zoom Out' );
    BtnViewZoomIn.Hint := R.ReadString( StockHintsSection, 'ZoomIn', 'Zoom In' );
    BtnAlign.Hint := R.ReadString( StockHintsSection, 'Align', 'Align' );
    BtnAlignLeft.Hint := R.ReadString( StockHintsSection, 'AlignLeft', 'Align Left' );
    BtnAlignTop.Hint := R.ReadString( StockHintsSection, 'AlignTop', 'Align Top' );
    BtnAlignRight.Hint := R.ReadString( StockHintsSection, 'AlignRight', 'Align Right' );
    BtnAlignBottom.Hint := R.ReadString( StockHintsSection, 'AlignBottom', 'Align Bottom' );
    BtnAlignClient.Hint := R.ReadString( StockHintsSection, 'AlignClient', 'Align Client' );
    BtnAlignNone.Hint := R.ReadString( StockHintsSection, 'AlignNone', 'Align None' );
    BtnFormatBold.Hint := R.ReadString( StockHintsSection, 'Bold', 'Bold' );
    BtnFormatItalics.Hint := R.ReadString( StockHintsSection, 'Italic', 'Italic' );
    BtnFormatUnderline.Hint := R.ReadString( StockHintsSection, 'Underline', 'Underline' );
    BtnFormatFont.Hint := R.ReadString( StockHintsSection, 'Font', 'Font' );
    BtnFormatLeft.Hint := R.ReadString( StockHintsSection, 'LeftJustify', 'Left Justify' );
    BtnFormatCenter.Hint := R.ReadString( StockHintsSection, 'CenterJustify', 'Center Justify' );
    BtnFormatRight.Hint := R.ReadString( StockHintsSection, 'RightJustify', 'Right Justify' );
    BtnFormatJustify.Hint := R.ReadString( StockHintsSection, 'Justify', 'Justify' );
    BtnFormatBullets.Hint := R.ReadString( StockHintsSection, 'Bullets', 'Bullets' );
    BtnFormatWordWrap.Hint := R.ReadString( StockHintsSection, 'WordWrap', 'Word Wrap' );
    BtnFormatTabs.Hint := R.ReadString( StockHintsSection, 'SetTabs', 'Set Tabs' );
    BtnOrderBackOne.Hint := R.ReadString( StockHintsSection, 'BackOne', 'Back One' );
    BtnOrderFrontOne.Hint := R.ReadString( StockHintsSection, 'ForwardOne', 'Forward One' );
    BtnOrderToBack.Hint := R.ReadString( StockHintsSection, 'SendToBack', 'Send to Back' );
    BtnOrderToFront.Hint := R.ReadString( StockHintsSection, 'BringToFront', 'Bring to Front' );
    BtnArrowLeft.Hint := R.ReadString( StockHintsSection, 'Left', 'Left' );
    BtnArrowRight.Hint := R.ReadString( StockHintsSection, 'Right', 'Right' );
    BtnArrowUp.Hint := R.ReadString( StockHintsSection, 'Up', 'Up' );
    BtnArrowDown.Hint := R.ReadString( StockHintsSection, 'Down', 'Down' );
    BtnMoveAllLeft.Hint := R.ReadString( StockHintsSection, 'MoveAllLeft', 'Move All Left' );
    BtnMoveLeft.Hint := R.ReadString( StockHintsSection, 'MoveLeft', 'Move Left' );
    BtnMoveRight.Hint := R.ReadString( StockHintsSection, 'MoveRight', 'Move Right' );
    BtnMoveAllRight.Hint := R.ReadString( StockHintsSection, 'MoveAllRight', 'Move All Right' );
    BtnMediaSkipBackward.Hint := R.ReadString( StockHintsSection, 'SkipBackward', 'Skip Backward' );
    BtnMediaRewind.Hint := R.ReadString( StockHintsSection, 'Rewind', 'Rewind' );
    BtnMediaPlay.Hint := R.ReadString( StockHintsSection, 'Play', 'Play' );
    BtnMediaPause.Hint := R.ReadString( StockHintsSection, 'Pause', 'Pause' );
    BtnMediaStop.Hint := R.ReadString( StockHintsSection, 'Stop', 'Stop' );
    BtnMediaRecord.Hint := R.ReadString( StockHintsSection, 'Record', 'Record' );
    BtnMediaFastForward.Hint := R.ReadString( StockHintsSection, 'FastForward', 'Fast Forward' );
    BtnMediaSkipForward.Hint := R.ReadString( StockHintsSection, 'SkipForward', 'Skip Forward' );
    BtnToolsSpeaker.Hint := R.ReadString( StockHintsSection, 'Volume', 'Volume' );
    BtnOK.Hint := R.ReadString( StockHintsSection, 'OK', 'OK' );
    BtnCancel.Hint := R.ReadString( StockHintsSection, 'Cancel', 'Cancel' );
    BtnSignalInfo.Hint := R.ReadString( StockHintsSection, 'Information', 'Information' );
    BtnSignalWarning.Hint := R.ReadString( StockHintsSection, 'Warning', 'Warning' );
    BtnSignalError.Hint := R.ReadString( StockHintsSection, 'Error', 'Error' );
    BtnSignalReminder.Hint := R.ReadString( StockHintsSection, 'Reminder', 'Reminder' );
    BtnSignalFinish.Hint := R.ReadString( StockHintsSection, 'Finish', 'Finish' );
    BtnSignalRed.Hint := R.ReadString( StockHintsSection, 'Red', '' );
    BtnSignalOrange.Hint := R.ReadString( StockHintsSection, 'Orange', '' );
    BtnSignalYellow.Hint := R.ReadString( StockHintsSection, 'Yellow', '' );
    BtnSignalGreen.Hint := R.ReadString( StockHintsSection, 'Green', '' );
    BtnSignalLtBlue.Hint := R.ReadString( StockHintsSection, 'LtBlue', '' );
    BtnSignalBlue.Hint := R.ReadString( StockHintsSection, 'Blue', '' );
    BtnSignalViolet.Hint := R.ReadString( StockHintsSection, 'Violet', '' );
    BtnSignalGray.Hint := R.ReadString( StockHintsSection, 'Gray', '' );
    BtnEmail.Hint := R.ReadString( StockHintsSection, 'SendEMail', 'Send EMail' );
    BtnAttach.Hint := R.ReadString( StockHintsSection, 'AttachFiles', 'Attach Files' );
    BtnNotebook.Hint := R.ReadString( StockHintsSection, 'Notebook', 'Note' );
    BtnNotePage.Hint := R.ReadString( StockHintsSection, 'NotePage', 'Note' );
    BtnNote.Hint := R.ReadString( StockHintsSection, 'Note', 'Note' );
    BtnToolsPen.Hint := R.ReadString( StockHintsSection, 'Pen', 'Edit' );
    BtnToolsPencil.Hint := R.ReadString( StockHintsSection, 'Pencil', 'Edit' );
    BtnToolsPin.Hint := R.ReadString( StockHintsSection, 'Pin', 'Attach' );
    BtnToolsRuler.Hint := R.ReadString( StockHintsSection, 'Ruler', 'Ruler' );
    BtnToolsCursor.Hint := R.ReadString( StockHintsSection, 'Select', 'Select' );
    BtnToolsHammer.Hint := R.ReadString( StockHintsSection, 'Utilities', 'Utilities' );
    BtnToolsKey.Hint := R.ReadString( StockHintsSection, 'Key', 'Security' );
    BtnToolsImage.Hint := R.ReadString( StockHintsSection, 'InsertImage', 'Insert Image' );
    BtnToolsPlug.Hint := R.ReadString( StockHintsSection, 'PlugIns', 'Plug-Ins' );
    BtnExecute.Hint := R.ReadString( StockHintsSection, 'Execute', 'Execute' );


    // Handle Custom Buttons
    BtnCount := R.ReadInteger( CustomButtonsSection, 'Count', 0 );
    for I := 0 to BtnCount - 1 do
    begin
      S := R.ReadString( CustomButtonsSection, 'Button' + IntToStr( I ), '' );
      if S <> '' then
      begin
        FCustomImages.AddObject( S, CreateCustomButton( S ) );
      end;

    end;
    RearrangeCustomButtons;

    ChkAddDisabled.Checked := R.ReadBool( ToolbarEditorSection, 'AddDisabled', True );

    // Read in Prefix
    FUsePrefix := R.ReadBool( ToolbarEditorSection, 'UsePrefix', True );
    FPrefix := R.ReadString( ToolbarEditorSection, 'Prefix', 'Btn' );
    if FUsePrefix then
      BtnChangePrefix.Caption := 'Prefix: ' + FPrefix
    else
      BtnChangePrefix.Caption := 'Suffix: ' + FPrefix;

    Width := R.ReadInteger( ToolbarEditorSection, 'Width', 697 );
    Height := R.ReadInteger( ToolbarEditorSection, 'Height', 478 );
  finally
    R.Free;
  end;
end;

procedure TRzToolbarEditDlg.FormDestroy(Sender: TObject);
var
  I: Integer;
  R: TRegIniFile;
begin
  R := TRegIniFile.Create( RC_SettingsKey );
  try
    // Save Stock Hints
    R.WriteString( StockHintsSection, 'New', BtnNew.Hint );
    R.WriteString( StockHintsSection, 'Open', BtnOpen.Hint );
    R.WriteString( StockHintsSection, 'Save', BtnSave.Hint );
    R.WriteString( StockHintsSection, 'SaveAll', BtnSaveAll.Hint );
    R.WriteString( StockHintsSection, 'Delete', BtnFileDelete.Hint );
    R.WriteString( StockHintsSection, 'Print', BtnPrint.Hint );
    R.WriteString( StockHintsSection, 'PrintPreview', BtnPrintPreview.Hint );
    R.WriteString( StockHintsSection, 'PreviewPreviousPage', BtnPreviewPrev.Hint );
    R.WriteString( StockHintsSection, 'PreviewNextPage', BtnPreviewNext.Hint );
    R.WriteString( StockHintsSection, 'Import', BtnImport.Hint );
    R.WriteString( StockHintsSection, 'Export', BtnExport.Hint );
    R.WriteString( StockHintsSection, 'Open', BtnFolderOpen.Hint );
    R.WriteString( StockHintsSection, 'Close', BtnFolderClosed.Hint );
    R.WriteString( StockHintsSection, 'UpOneLevel', BtnFolderUp.Hint );
    R.WriteString( StockHintsSection, 'NewFolder', BtnFolderNew.Hint );
    R.WriteString( StockHintsSection, 'SelectFolder', BtnFolderSelect.Hint );
    R.WriteString( StockHintsSection, 'Exit', BtnExit.Hint );
    R.WriteString( StockHintsSection, 'Cut', BtnCut.Hint );
    R.WriteString( StockHintsSection, 'Copy', BtnCopy.Hint );
    R.WriteString( StockHintsSection, 'Paste', BtnPaste.Hint );
    R.WriteString( StockHintsSection, 'Undo', BtnUndo.Hint );
    R.WriteString( StockHintsSection, 'Redo', BtnRedo.Hint );
    R.WriteString( StockHintsSection, 'SelectAll', BtnSelectAll.Hint );
    R.WriteString( StockHintsSection, 'Clear', BtnClear.Hint );
    R.WriteString( StockHintsSection, 'Recycle', BtnRecycle.Hint );
    R.WriteString( StockHintsSection, 'Recycle', BtnRecycleXP.Hint );
    R.WriteString( StockHintsSection, 'CopyAll', BtnCopyAll.Hint );
    R.WriteString( StockHintsSection, 'Move', BtnMove.Hint );
    R.WriteString( StockHintsSection, 'Properties', BtnProperties.Hint );
    R.WriteString( StockHintsSection, 'ChangeOptions', BtnOptions.Hint );
    R.WriteString( StockHintsSection, 'DateView', BtnCalendarDate.Hint );
    R.WriteString( StockHintsSection, 'WeekView', BtnCalendarWeek.Hint );
    R.WriteString( StockHintsSection, 'MonthView', BtnCalendarMonth.Hint );
    R.WriteString( StockHintsSection, 'Find', BtnSearchFind.Hint );
    R.WriteString( StockHintsSection, 'Replace', BtnSearchReplace.Hint );
    R.WriteString( StockHintsSection, 'FindNext', BtnSearchFindNext.Hint );
    R.WriteString( StockHintsSection, 'JumpToLine', BtnSearchJumpToLine.Hint );
    R.WriteString( StockHintsSection, 'FirstRecord', BtnDBFirst.Hint );
    R.WriteString( StockHintsSection, 'PreviousRecord', BtnDBPrevious.Hint );
    R.WriteString( StockHintsSection, 'NextRecord', BtnDBNext.Hint );
    R.WriteString( StockHintsSection, 'LastRecord', BtnDBLast.Hint );
    R.WriteString( StockHintsSection, 'InsertRecord', BtnDBInsert.Hint );
    R.WriteString( StockHintsSection, 'DeleteRecord', BtnDBDelete.Hint );
    R.WriteString( StockHintsSection, 'EditRecord', BtnDBEdit.Hint );
    R.WriteString( StockHintsSection, 'PostRecord', BtnDBPost.Hint );
    R.WriteString( StockHintsSection, 'CancelChanges', BtnDBCancel.Hint );
    R.WriteString( StockHintsSection, 'Refresh', BtnDBRefresh.Hint );
    R.WriteString( StockHintsSection, 'Help', BtnHelp.Hint );
    R.WriteString( StockHintsSection, 'HelpBook', BtnHelpBook.Hint );
    R.WriteString( StockHintsSection, 'ContextHelp', BtnHelpContext.Hint );
    R.WriteString( StockHintsSection, 'Internet', BtnNetWeb.Hint );
    R.WriteString( StockHintsSection, 'Newsgroups', BtnNetNews.Hint );
    R.WriteString( StockHintsSection, 'Cascade', BtnWindowCascade.Hint );
    R.WriteString( StockHintsSection, 'HorizTile', BtnWindowHorzTile.Hint );
    R.WriteString( StockHintsSection, 'VertTile', BtnWindowVertTile.Hint );
    R.WriteString( StockHintsSection, 'Tile', BtnWindowTile.Hint );
    R.WriteString( StockHintsSection, 'ViewIcons', BtnViewIcons.Hint );
    R.WriteString( StockHintsSection, 'ViewList', BtnViewList.Hint );
    R.WriteString( StockHintsSection, 'ViewDetails', BtnViewDetails.Hint );
    R.WriteString( StockHintsSection, 'View', BtnViewZoom.Hint );
    R.WriteString( StockHintsSection, 'ZoomOut', BtnViewZoomOut.Hint );
    R.WriteString( StockHintsSection, 'ZoomIn', BtnViewZoomIn.Hint );
    R.WriteString( StockHintsSection, 'Align', BtnAlign.Hint );
    R.WriteString( StockHintsSection, 'AlignLeft', BtnAlignLeft.Hint );
    R.WriteString( StockHintsSection, 'AlignTop', BtnAlignTop.Hint );
    R.WriteString( StockHintsSection, 'AlignRight', BtnAlignRight.Hint );
    R.WriteString( StockHintsSection, 'AlignBottom', BtnAlignBottom.Hint );
    R.WriteString( StockHintsSection, 'AlignClient', BtnAlignClient.Hint );
    R.WriteString( StockHintsSection, 'AlignNone', BtnAlignNone.Hint );
    R.WriteString( StockHintsSection, 'Bold', BtnFormatBold.Hint );
    R.WriteString( StockHintsSection, 'Italic', BtnFormatItalics.Hint );
    R.WriteString( StockHintsSection, 'Underline', BtnFormatUnderline.Hint );
    R.WriteString( StockHintsSection, 'Font', BtnFormatFont.Hint );
    R.WriteString( StockHintsSection, 'LeftJustify', BtnFormatLeft.Hint );
    R.WriteString( StockHintsSection, 'CenterJustify', BtnFormatCenter.Hint );
    R.WriteString( StockHintsSection, 'RightJustify', BtnFormatRight.Hint );
    R.WriteString( StockHintsSection, 'Justify', BtnFormatJustify.Hint );
    R.WriteString( StockHintsSection, 'Bullets', BtnFormatBullets.Hint );
    R.WriteString( StockHintsSection, 'WordWrap', BtnFormatWordWrap.Hint );
    R.WriteString( StockHintsSection, 'SetTabs', BtnFormatTabs.Hint );
    R.WriteString( StockHintsSection, 'BackOne', BtnOrderBackOne.Hint );
    R.WriteString( StockHintsSection, 'ForwardOne', BtnOrderFrontOne.Hint );
    R.WriteString( StockHintsSection, 'SendToBack', BtnOrderToBack.Hint );
    R.WriteString( StockHintsSection, 'BringToFront', BtnOrderToFront.Hint );
    R.WriteString( StockHintsSection, 'Left', BtnArrowLeft.Hint );
    R.WriteString( StockHintsSection, 'Right', BtnArrowRight.Hint );
    R.WriteString( StockHintsSection, 'Up', BtnArrowUp.Hint );
    R.WriteString( StockHintsSection, 'Down', BtnArrowDown.Hint );
    R.WriteString( StockHintsSection, 'MoveAllLeft', BtnMoveAllLeft.Hint );
    R.WriteString( StockHintsSection, 'MoveLeft', BtnMoveLeft.Hint );
    R.WriteString( StockHintsSection, 'MoveRight', BtnMoveRight.Hint );
    R.WriteString( StockHintsSection, 'MoveAllRight', BtnMoveAllRight.Hint );
    R.WriteString( StockHintsSection, 'SkipBackward', BtnMediaSkipBackward.Hint );
    R.WriteString( StockHintsSection, 'Rewind', BtnMediaRewind.Hint );
    R.WriteString( StockHintsSection, 'Play', BtnMediaPlay.Hint );
    R.WriteString( StockHintsSection, 'Pause', BtnMediaPause.Hint );
    R.WriteString( StockHintsSection, 'Stop', BtnMediaStop.Hint );
    R.WriteString( StockHintsSection, 'Record', BtnMediaRecord.Hint );
    R.WriteString( StockHintsSection, 'FastForward', BtnMediaFastForward.Hint );
    R.WriteString( StockHintsSection, 'SkipForward', BtnMediaSkipForward.Hint );
    R.WriteString( StockHintsSection, 'Volume', BtnToolsSpeaker.Hint );
    R.WriteString( StockHintsSection, 'OK', BtnOK.Hint );
    R.WriteString( StockHintsSection, 'Cancel', BtnCancel.Hint );
    R.WriteString( StockHintsSection, 'Information', BtnSignalInfo.Hint );
    R.WriteString( StockHintsSection, 'Warning', BtnSignalWarning.Hint );
    R.WriteString( StockHintsSection, 'Error', BtnSignalError.Hint );
    R.WriteString( StockHintsSection, 'Reminder', BtnSignalReminder.Hint );
    R.WriteString( StockHintsSection, 'Finish', BtnSignalFinish.Hint );
    R.WriteString( StockHintsSection, 'Red', BtnSignalRed.Hint );
    R.WriteString( StockHintsSection, 'Orange', BtnSignalOrange.Hint );
    R.WriteString( StockHintsSection, 'Yellow', BtnSignalYellow.Hint );
    R.WriteString( StockHintsSection, 'Green', BtnSignalGreen.Hint );
    R.WriteString( StockHintsSection, 'LtBlue', BtnSignalLtBlue.Hint );
    R.WriteString( StockHintsSection, 'Blue', BtnSignalBlue.Hint );
    R.WriteString( StockHintsSection, 'Violet', BtnSignalViolet.Hint );
    R.WriteString( StockHintsSection, 'Gray', BtnSignalGray.Hint );
    R.WriteString( StockHintsSection, 'SendEMail', BtnEmail.Hint );
    R.WriteString( StockHintsSection, 'AttachFiles', BtnAttach.Hint );
    R.WriteString( StockHintsSection, 'Notebook', BtnNotebook.Hint );
    R.WriteString( StockHintsSection, 'NotePage', BtnNotePage.Hint );
    R.WriteString( StockHintsSection, 'Note', BtnNote.Hint );
    R.WriteString( StockHintsSection, 'Pen', BtnToolsPen.Hint );
    R.WriteString( StockHintsSection, 'Pencil', BtnToolsPencil.Hint );
    R.WriteString( StockHintsSection, 'Pin', BtnToolsPin.Hint );
    R.WriteString( StockHintsSection, 'Ruler', BtnToolsRuler.Hint );
    R.WriteString( StockHintsSection, 'Select', BtnToolsCursor.Hint );
    R.WriteString( StockHintsSection, 'Utilities', BtnToolsHammer.Hint );
    R.WriteString( StockHintsSection, 'Key', BtnToolsKey.Hint );
    R.WriteString( StockHintsSection, 'InsertImage', BtnToolsImage.Hint );
    R.WriteString( StockHintsSection, 'PlugIns', BtnToolsPlug.Hint );
    R.WriteString( StockHintsSection, 'Execute', BtnExecute.Hint );

    // Save Custom Button Information
    R.WriteInteger( CustomButtonsSection, 'Count', FCustomImages.Count );
    for I := 0 to FCustomImages.Count - 1 do
    begin
      R.WriteString( CustomButtonsSection, 'Button' + IntToStr( I ), FCustomImages[ I ] );
    end;

    R.WriteBool( ToolbarEditorSection, 'UsePrefix', FUsePrefix );
    R.WriteString( ToolbarEditorSection, 'Prefix', FPrefix );
    R.WriteBool( ToolbarEditorSection, 'AddDisabled', ChkAddDisabled.Checked );
    R.WriteInteger( ToolbarEditorSection, 'Width', Width );
    R.WriteInteger( ToolbarEditorSection, 'Height', Height );
  finally
    R.Free;
  end;

  FCustomImages.Free;
end;



procedure TRzToolbarEditDlg.Reposition;
var
  T, L: Integer;
  R, DeskRect: TRect;
  P: TPoint;
begin
  if Toolbar <> nil then
  begin
    R := Toolbar.BoundsRect;
    case Toolbar.Align of
      alNone:
        P := Point( R.Left, R.Bottom );
      alLeft:
        P := Point( R.Right, R.Top );
      alTop:
        P := Point( R.Left, R.Bottom );
      alRight:
        P := Point( R.Left - Width - 5, R.Top );
      alBottom:
        P := Point( R.Left, R.Top - Height - 5 );
    end;

    P := GetParentForm( Toolbar ).ClientToScreen( P );

    T := P.Y + 25;
    L := P.X + 15;
    DeskRect := GetDesktopClientRect;
    if L < 0 then
      L := 0;
    if L + Width > DeskRect.Right then
      L := DeskRect.Right - Width;

    if T < 0 then
      T := 0;
    if T + Height > DeskRect.Bottom then
      T := DeskRect.Bottom - Height;

    Top := T;
    Left := L;
  end;
end;


procedure TRzToolbarEditDlg.UpdateControls;
var
  I: Integer;
begin
  for I := 0 to Toolbar.ToolbarControls.Count - 1 do
    LstOrder.Items.Add( TRzToolbarControl( Toolbar.ToolbarControls[ I ] ).Control.Name );

  BtnMoveUp.Enabled := LstOrder.Items.Count > 0;
  BtnMoveDown.Enabled := LstOrder.Items.Count > 0;
  if LstOrder.Items.Count > 0 then
  begin
    LstOrder.ItemIndex := 0;
    EnableMoveButtons( 0 );
  end;

  if Toolbar.Images = nil then
  begin
    GrpStockImages.Enabled := False;
    GrpCustomImages.Enabled := False;
    for I := 0 to FCustomImages.Count - 1 do
      TRzToolbarButton( FCustomImages.Objects[ I ] ).Enabled := False;
    LblNoImageListTitle.Visible := True;
    LblNoImageListTitle.Enabled := True;
    LblNoImageListTitle.Height := 120;
    LblNoImageListTitle.BringToFront;
    LblNoImageListMsg.Caption := Format( LblNoImageListMsg.Caption, [ Toolbar.Name ] );
    LblNoImageListMsg.Visible := True;
    LblNoImageListMsg.Enabled := True;
    LblNoImageListMsg.Height := 190;
    LblNoImageListMsg.BringToFront;
  end;
end;


procedure TRzToolbarEditDlg.EnableMoveButtons( Idx: Integer );
begin
  BtnMoveUp.Enabled := Idx > 0;
  BtnMoveDown.Enabled := Idx < LstOrder.Items.Count - 1;
end;


function TRzToolbarEditDlg.CreateNewButton( const Hint: string ): TRzToolButton;
begin
  Result := TRzToolButton.Create( CompOwner );
  Result.Parent := Toolbar;
  if FUsePrefix then
    Result.Name := GetNewComponentName( CompOwner, CreateValidIdent( FPrefix + Hint, 'RzToolButton' ) )
  else
    Result.Name := GetNewComponentName( CompOwner, CreateValidIdent( Hint + FPrefix, 'RzToolButton' ) );
  Result.Caption := Hint;
  Result.Hint := Hint;
end;


procedure TRzToolbarEditDlg.AddImageToImageList( Btn: TRzToolButton; Glyph: TBitmap );
var
  B: TBitmap;
  R: TRect;
  I: Integer;
  F: TForm;
  DM: TDataModule;
begin
  if ( Btn.ImageList <> nil ) and ( Glyph <> nil ) then
  begin
    if Glyph.Width = Glyph.Height then
    begin
      // Easy case -- Only one glyph in bitmap (i.e. no disabled glyph)
      Btn.ImageList.AddMasked( Glyph, Glyph.Canvas.Pixels[ 0, 15 ] );
      Btn.ImageIndex := Btn.ImageList.Count - 1;
    end
    else
    begin
      // Assume Glyph has both a normal image and a disabled image
      if ChkAddDisabled.Checked then
      begin
        Btn.ImageList.AddMasked( Glyph, Glyph.Canvas.Pixels[ 0, 15 ] );
        Btn.DisabledIndex := Btn.ImageList.Count - 1;
        Btn.ImageIndex := Btn.ImageList.Count - 2;
      end
      else
      begin
        // Extract out the normal image from Glyph
        B := TBitmap.Create;
        try
          B.Width := Btn.ImageList.Width;
          B.Height := Btn.ImageList.Height;
          R := Rect( 0, 0, B.Width, B.Height );
          B.Canvas.CopyRect( R, Glyph.Canvas, R );
          Btn.ImageList.AddMasked( B, B.Canvas.Pixels[ 0, 15 ] );
          Btn.ImageIndex := Btn.ImageList.Count - 1;
        finally
          B.Free;
        end;
      end;
    end;
    
    if ( Btn.ImageList.Owner <> nil ) and ( Btn.ImageList.Owner is TDataModule ) then
    begin
      // If the ImageList is on a DataModule, we must get to the DataModule's
      // designer and tell it that is has been modified.
      
      DM := TDataModule( Btn.ImageList.Owner );
      
      for I := 0 to Screen.FormCount - 1 do
      begin
        F := Screen.Forms[ I ];
        if ( F.Caption = DM.Name ) and ( F.ClassName = 'TDataModuleForm' ) then
        begin
          if F.Designer <> nil then
            F.Designer.Modified;
          Break;
        end;
      end;
    end;
    
  end;
end; {= TRzToolbarEditDlg.AddImageToImageList =}


procedure TRzToolbarEditDlg.ToolbarButtonClick(Sender: TObject);
var
  Btn: TRzToolButton;
  I: Integer;
begin
  if CompOwner <> nil then
  begin
    SelectedBtn := TRzToolbarButton( Sender );
    Btn := CreateNewButton( SelectedBtn.Hint );
    AddImageToImageList( Btn, SelectedBtn.Glyph );

    I := Toolbar.ToolbarControls.Count - 1;
    LstOrder.Items.Add( TRzToolbarControl( Toolbar.ToolbarControls[ I ] ).Control.Name );
  end;
end;



procedure TRzToolbarEditDlg.BtnSpacerClick(Sender: TObject);
var
  Spacer: TRzSpacer;
  I: Integer;
begin
  if CompOwner <> nil then
  begin
    Spacer := TRzSpacer.Create( CompOwner );
    Spacer.Parent := Toolbar;
    Spacer.Name := GetNewComponentName( CompOwner, 'RzSpacer', False );
    Spacer.Grooved := Sender = BtnGrooveSpacer;
    I := Toolbar.ToolbarControls.Count - 1;
    LstOrder.Items.Add( TRzToolbarControl( Toolbar.ToolbarControls[ I ] ).Control.Name );
  end;
end;



procedure TRzToolbarEditDlg.RearrangeCustomButtons;
var
  B: TRzToolbarButton;
  I, N: Integer;
begin
  for I := 0 to FCustomImages.Count - 1 do
  begin
    B := FCustomImages.Objects[ I ] as TRzToolbarButton;
    N := SbxCustom.Width div 25;
    B.Left := ( I mod N ) * 25;
    B.Top := ( I div N ) * 25;
  end;
end;


function TRzToolbarEditDlg.CreateCustomButton( const S: string ): TRzToolbarButton;
var
  N: Integer;
begin
  Result := TRzToolbarButton.Create( Self );
  Result.Parent := SbxCustom;
  Result.Glyph.LoadFromFile( GetFileNameFromString( S ) );

  N := 1;
  if ( Result.Glyph.Height <> 0 ) and ( Result.Glyph.Width mod Result.Glyph.Height = 0 ) then
  begin
    N := Result.Glyph.Width div Result.Glyph.Height;
    if N > 4 then
      N := 1;
  end;
  Result.NumGlyphs := N;

  Result.Hint := GetHintFromString( S );
  Result.PopupMenu := MnuCustom;
  Result.OnClick := ToolbarButtonClick;
end;


function TRzToolbarEditDlg.GetFileNameFromString( const S: string ): string;
begin
  Result := Copy( S, Pos( '|', S ) + 1, 255 );
end;


function TRzToolbarEditDlg.GetHintFromString( const S: string ): string;
begin
  Result := Copy( S, 1, Pos( '|', S ) - 1 );
end;


procedure TRzToolbarEditDlg.MnuAddClick(Sender: TObject);
var
  S, FName, HintStr: string;
begin
  DlgOpenPicture.FileName := '';
  if DlgOpenPicture.Execute then
  begin
    FName := DlgOpenPicture.FileName;
    HintStr := LowerCase( ChangeFileExt( ExtractFileName( FName ), '' ) );
    HintStr[ 1 ] := UpCase( HintStr[ 1 ] );

    S := HintStr + '|' + FName;
    FCustomImages.AddObject( S, CreateCustomButton( S ) );
    RearrangeCustomButtons;
  end;
end;


procedure TRzToolbarEditDlg.MnuDeleteClick(Sender: TObject);
var
  S: string;
  Idx: Integer;
begin
  SelectedBtn := MnuCustom.PopupComponent as TRzToolbarButton;
  S := Format( 'Delete %s custom image?', [ SelectedBtn.Hint ] );
  if MessageDlg( S, mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes then
  begin
    Idx := FCustomImages.IndexOfObject( SelectedBtn );
    SelectedBtn.Free;
    if Idx <> -1 then
      FCustomImages.Delete( Idx );
    RearrangeCustomButtons;
  end;
end;


procedure TRzToolbarEditDlg.MnuEditCustomHintClick(Sender: TObject);
var
  NewHint: string;
begin
  SelectedBtn := MnuCustom.PopupComponent as TRzToolbarButton;
  if SelectedBtn <> nil then
  begin
    NewHint := SelectedBtn.Hint;
    if InputQuery( 'Custom Image Hint', 'Enter New Hint', NewHint ) then
      SelectedBtn.Hint := NewHint;
  end;
end;


procedure TRzToolbarEditDlg.MnuEditStockHintClick(Sender: TObject);
var
  NewHint: string;
begin
  SelectedBtn := MnuStock.PopupComponent as TRzToolbarButton;
  if SelectedBtn <> nil then
  begin
    NewHint := SelectedBtn.Hint;
    if InputQuery( 'Stock Image Hint', 'Enter New Hint', NewHint ) then
      SelectedBtn.Hint := NewHint;
  end;
end;


procedure TRzToolbarEditDlg.SbxCustomResize(Sender: TObject);
begin
  RearrangeCustomButtons;
end;


procedure TRzToolbarEditDlg.BtnMoveUpClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := LstOrder.ItemIndex;
  if Idx <> -1 then
  begin
    LstOrder.Items.BeginUpdate;
    try
      LstOrder.Items.Move( Idx, Idx - 1 );
      Toolbar.ToolbarControls.Move( Idx, Idx - 1 );
      Toolbar.PositionControls;
      LstOrder.ItemIndex := Idx - 1;
    finally
      LstOrder.Invalidate;
      LstOrder.Items.EndUpdate;
    end;
    EnableMoveButtons( Idx - 1 );
  end;
end;

procedure TRzToolbarEditDlg.BtnMoveDownClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := LstOrder.ItemIndex;
  if Idx <> -1 then
  begin
    LstOrder.Items.BeginUpdate;
    try
      LstOrder.Items.Move( Idx, Idx + 1 );
      Toolbar.ToolbarControls.Move( Idx, Idx + 1 );
      Toolbar.PositionControls;
      LstOrder.ItemIndex := Idx + 1;
    finally
      LstOrder.Invalidate;
      LstOrder.Items.EndUpdate;
    end;
    EnableMoveButtons( Idx + 1 );
  end;
end;

procedure TRzToolbarEditDlg.BtnDeleteControlClick(Sender: TObject);
var
  Idx: Integer;
  C: TControl;
begin
  Idx := LstOrder.ItemIndex;
  if Idx <> -1 then
  begin
    C := Toolbar.ToolbarControls.Items[ Idx ].Control;
    Toolbar.RemoveControl( C );
    C.Free;

    // Use RzListBox.Delete method so ItemIndex gets updated accordingly afte the deletion.
    LstOrder.Delete( Idx );
  end;
end;


procedure TRzToolbarEditDlg.BtnChangePrefixClick(Sender: TObject);
var
  F: TRzFrmPrefixSuffix;
begin
  F := TRzFrmPrefixSuffix.Create( Application );
  try
    F.OptPrefix.Checked := FUsePrefix;
    F.OptSuffix.Checked := not FUsePrefix;
    F.EdtPrefix.Text := FPrefix;
    if F.ShowModal = mrOK then
    begin
      FUsePrefix := F.OptPrefix.Checked;
      FPrefix := F.EdtPrefix.Text;
      if FUsePrefix then
        BtnChangePrefix.Caption := 'Prefix: ' + FPrefix
      else
        BtnChangePrefix.Caption := 'Suffix: ' + FPrefix;
    end;
  finally
    F.Free;
  end;
end;


procedure TRzToolbarEditDlg.MnuPrefixClick(Sender: TObject);
begin
  FUsePrefix := True;
end;

procedure TRzToolbarEditDlg.MnuSuffixClick(Sender: TObject);
begin
  FUsePrefix := False;
end;



procedure TRzToolbarEditDlg.LstOrderClick(Sender: TObject);
begin
  EnableMoveButtons( LstOrder.ItemIndex );
end;


procedure TRzToolbarEditDlg.LstOrderDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if NewIdx <> -1 then
  begin
    LstOrder.Items.BeginUpdate;
    try
      LstOrder.Items.Move( OldIdx, NewIdx );
      Toolbar.ToolbarControls.Move( OldIdx, NewIdx );
      Toolbar.PositionControls;
      LstOrder.ItemIndex := NewIdx;
      EnableMoveButtons( LstOrder.ItemIndex );
    finally
      LstOrder.Invalidate;
      LstOrder.Items.EndUpdate;
    end;
  end;
end;


procedure TRzToolbarEditDlg.LstOrderDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  NewIdx := ( Sender as TRzListBox ).ItemAtPos( Point( X, Y ), True );
  Accept := ( Source = Sender ) and ( NewIdx <> -1 );
  if Accept then
  begin
    with Sender as TRzListBox do
    begin
      if Y > Height - ItemHeight then
      begin
        GoingUp := False;
        Timer1.Enabled := True
      end
      else if Y < ItemHeight then
      begin
        GoingUp := True;
        Timer1.Enabled := True
      end
      else
        Timer1.Enabled := False;
      ItemIndex := NewIdx;
    end;
  end;
end;


procedure TRzToolbarEditDlg.LstOrderDrawItem( Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState );
var
  Bmp: TBitmap;
  TextOffset: Integer;
  R, DestRct, SrcRct: TRect;
  TransColor: TColor;
  SB: TSpeedButton;
  TB: TRzToolButton;
  SP: TRzSpacer;

  function GetTransparentColor( B: TBitmap ): TColor;
  begin
    Result := B.Canvas.Pixels[ 0, B.Height - 1 ];
  end;

begin
  LstOrder.Canvas.FillRect( Rect );                   { Clear area for icon and text }

  if TRzToolbarControl( Toolbar.ToolbarControls[ Index ] ).Control is TRzToolButton then
  begin
    TB := TRzToolbarControl( Toolbar.ToolbarControls[ Index ] ).Control as TRzToolButton;
    if TB.ImageList <> nil then
    begin
      TB.ImageList.Draw( LstOrder.Canvas, Rect.Left - 22, Rect.Top + 2, TB.ImageIndex );
    end;
  end
  else if TRzToolbarControl( Toolbar.ToolbarControls[ Index ] ).Control is TSpeedButton then
  begin
    SB := TRzToolbarControl( Toolbar.ToolbarControls[ Index ] ).Control as TSpeedButton;

    Bmp := TBitmap.Create;
    try

      DestRct := Classes.Rect( 0, 0, SB.Glyph.Width div SB.NumGlyphs, SB.Glyph.Height );
      SrcRct := DestRct;

      { Don't forget to set the Width and Height of destination bitmap. }
      Bmp.Width := SB.Glyph.Width div SB.NumGlyphs;
      Bmp.Height := SB.Glyph.Height;

      TransColor := GetTransparentColor( SB.Glyph );
      Bmp.Canvas.BrushCopy( DestRct, SB.Glyph, SrcRct, TransColor);

      LstOrder.Canvas.Draw( Rect.Left - 22, Rect.Top + 2, Bmp );
    finally
      Bmp.Free;
    end;
  end
  else if TRzToolbarControl( Toolbar.ToolbarControls[ Index ] ).Control is TRzSpacer then
  begin
    SP := TRzToolbarControl( Toolbar.ToolbarControls[ Index ] ).Control as TRzSpacer;

    Bmp := TBitmap.Create;
    try

      DestRct := Classes.Rect( 0, 0, 16, 16 );
      SrcRct := DestRct;

      { Don't forget to set the Width and Height of destination bitmap. }
      Bmp.Width := 16;
      Bmp.Height := 16;

      TransColor := GetTransparentColor( BtnSpacer.Glyph );
      if SP.Grooved then
        Bmp.Canvas.BrushCopy( DestRct, BtnGrooveSpacer.Glyph, SrcRct, TransColor )
      else
        Bmp.Canvas.BrushCopy( DestRct, BtnSpacer.Glyph, SrcRct, TransColor );

      LstOrder.Canvas.Draw( Rect.Left - 22, Rect.Top + 2, Bmp );
    finally
      Bmp.Free;
    end;
  end;


  R := Rect;
//  Inc( R.Left, 30 );
  TextOffset := ( LstOrder.ItemHeight - Canvas.TextHeight( 'Pp' ) ) div 2;
  LstOrder.Canvas.TextRect( R, R.Left + 2, R.Top + TextOffset, LstOrder.Items[ Index ] );
end;


procedure TRzToolbarEditDlg.LstOrderEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  Timer1.Enabled := False;
end;


procedure TRzToolbarEditDlg.LstOrderMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OldIdx := LstOrder.ItemIndex;
end;


procedure TRzToolbarEditDlg.Timer1Timer(Sender: TObject);
begin
  with LstOrder do
  begin
    if GoingUp then
      if TopIndex > 0 then
        TopIndex := TopIndex - 1
      else
        Timer1.Enabled := False
    else
      if TopIndex < Items.Count - 1 then
        TopIndex := TopIndex + 1
      else
        Timer1.Enabled := False;
  end;
end;


procedure TRzToolbarEditDlg.ButtonStyleClick( Sender: TObject );
begin
  if Toolbar <> nil then
  begin
    if Sender = BtnNoCaptionsStyle then
      Toolbar.TextOptions := ttoNoTextLabels
    else if Sender = BtnCaptionsBottomStyle then
      Toolbar.TextOptions := ttoShowTextLabels
    else
      Toolbar.TextOptions := ttoSelectiveTextOnRight;
  end;
end;


procedure TRzToolbarEditDlg.FormResize(Sender: TObject);
begin
  GrpControls.Height := ClientHeight - 74;
  LstOrder.Height := GrpControls.Height - 56;
  GrpCustomImages.Width := ClientWidth - 223;
  GrpCustomImages.Height := ClientHeight - 334;
end;

end.




