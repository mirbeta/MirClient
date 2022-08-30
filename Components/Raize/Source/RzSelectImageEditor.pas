{===============================================================================
  RzSelectImageEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzSelectImageEditor
    Provides a quick way of selecting glyphs for TRzToolarButton and 
    TRzToolButton components.

  TRzToolButtonEditor
    Adds a context menu to quickly change several properties of a TRzToolButton.

  TRzBitBtnEditor
    Adds a context menu to quickly change several properties of a TRzMenuButton.

  TRzMenuButtonEditor
    Adds a context menu to quickly change several properties of a TRzMenuButton.


  TRzCustomImageIndexProperty
    Displays image index values (and each image) associated with an ImageList.

  TRzToolButtonImageIndexProperty
    Property editor for TRzToolButton.ImageIndex property

  TRzTabSheetImageIndexProperty
    Property editor for TRzTabSheet.ImageIndex property

  TRzGroupCaptionImageIndexProperty
    Property editor for TRzGroup.CaptionImageIndex property

  TRzGroupItemImageIndexProperty
    Property editor for TRzGroupItem.ImageIndex property

  TRzPathItemImageIndexProperty
    Property editor for TRzPathItem.ImageIndex property

  TRzGroupTemplateCaptionImageIndexProperty
    Property editor for TRzGroupTemplate.CaptionImageIndex property

  TRzGroupTemplateItemImageIndexProperty
    Property editor for TRzGroupTemplateItem.ImageIndex property

  TRzAnimatorImageIndexProperty
    Property editor for TRzAnimator.ImageIndex property

  TRzTrayIconIndexProperty
    Property editor for TRzTrayIcon.IconIndex property


  Modification History
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed positioning of Select Image Editor so that the design editor appears
      just under the control being edited if possible.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Reconnected Edit Hint and Delete popup menu items in the Custom images
      group box.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzSelectImageEditor;

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
  DesignMenus,
  DesignEditors,
  VCLEditors,
  Forms,
  Dialogs,
  Buttons,
  StdCtrls,
  RzPanel,
  ExtCtrls,
  ImgList,
  Menus,
  RzButton,
  RzDesignEditors,
  RzRadChk,
  RzLstBox,
  RzLabel,
  RzStatus,
  RzSpnEdt,
  ExtDlgs;

type
  {===================================================}
  {== TRzCustomImageIndexProperty Class Declaration ==}
  {===================================================}

  TRzCustomImageIndexProperty = class( TIntegerProperty, ICustomPropertyListDrawing )
  protected
    {** Descendant class must override GetImageList **}
    function GetImageList: TCustomImageList; virtual; abstract;

    function GetImageHeight( Images: TCustomImageList; ACanvas: TCanvas ): Integer; virtual;
    function GetImageWidth( Images: TCustomImageList; ACanvas: TCanvas ): Integer; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues( Proc: TGetStrProc ); override;
    procedure Edit; override;

    // ICustomPropertyListDrawing
    procedure ListMeasureHeight( const Value: string; ACanvas: TCanvas; var AHeight: Integer );
    procedure ListMeasureWidth( const Value: string; ACanvas: TCanvas; var AWidth: Integer );
    procedure ListDrawValue( const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean );
  end;



  {=============================================}
  {== TRzImageIndexProperty Class Declaration ==}
  {=============================================}

  TRzImageIndexProperty = class( TRzCustomImageIndexProperty )
  protected
    function GetImageList: TCustomImageList; override;
  end;


  {=======================================================}
  {== TRzToolButtonImageIndexProperty Class Declaration ==}
  {=======================================================}

  TRzToolButtonImageIndexProperty = class( TRzCustomImageIndexProperty )
  protected
    function GetImageList: TCustomImageList; override;
  end;


  {=====================================================}
  {== TRzTabSheetImageIndexProperty Class Declaration ==}
  {=====================================================}

  TRzTabSheetImageIndexProperty = class( TRzCustomImageIndexProperty )
  protected
    function GetImageList: TCustomImageList; override;
  end;


  {=========================================================}
  {== TRzGroupCaptionImageIndexProperty Class Declaration ==}
  {=========================================================}

  TRzGroupCaptionImageIndexProperty = class( TRzCustomImageIndexProperty )
  protected
    function GetImageList: TCustomImageList; override;
  end;


  {======================================================}
  {== TRzGroupItemImageIndexProperty Class Declaration ==}
  {======================================================}

  TRzGroupItemImageIndexProperty = class( TRzCustomImageIndexProperty )
  protected
    function GetImageList: TCustomImageList; override;
  end;


  {======================================================}
  {== TRzPathItemImageIndexProperty Class Declaration ==}
  {======================================================}

  TRzPathItemImageIndexProperty = class( TRzCustomImageIndexProperty )
  protected
    function GetImageList: TCustomImageList; override;
  end;
  
  
  {=================================================================}
  {== TRzGroupTemplateCaptionImageIndexProperty Class Declaration ==}
  {=================================================================}

  TRzGroupTemplateCaptionImageIndexProperty = class( TRzCustomImageIndexProperty )
  protected
    function GetImageList: TCustomImageList; override;
  end;


  {==============================================================}
  {== TRzGroupTemplateItemImageIndexProperty Class Declaration ==}
  {==============================================================}

  TRzGroupTemplateItemImageIndexProperty = class( TRzCustomImageIndexProperty )
  protected
    function GetImageList: TCustomImageList; override;
  end;


  {=====================================================}
  {== TRzAnimatorImageIndexProperty Class Declaration ==}
  {=====================================================}

  TRzAnimatorImageIndexProperty = class( TRzCustomImageIndexProperty )
  protected
    function GetImageList: TCustomImageList; override;
  end;


  {================================================}
  {== TRzTrayIconIndexProperty Class Declaration ==}
  {================================================}

  TRzTrayIconIndexProperty = class( TRzCustomImageIndexProperty )
  protected
    function GetImageList: TCustomImageList; override;
  end;


  {===========================================}
  {== TRzToolButtonEditor Class Declaration ==}
  {===========================================}

  TRzToolButtonEditor = class( TRzDefaultEditor )
  protected
    function ToolButton: TRzToolButton;
    function GetCompRefData( Index: Integer; var CompRefClass: TComponentClass; var CompRefPropName: string;
                             var CompRefMenuHandler: TNotifyEvent ): Boolean; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure LayoutMenuHandler( Sender: TObject );
    procedure DropDownMenuMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {=======================================}
  {== TRzBitBtnEditor Class Declaration ==}
  {=======================================}

  TRzBitBtnEditor = class( TRzDefaultEditor )
  protected
    function BitBtn: TRzBitBtn;
    function GetCompRefData( Index: Integer; var CompRefClass: TComponentClass; var CompRefPropName: string;
                             var CompRefMenuHandler: TNotifyEvent ): Boolean; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ): string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {===========================================}
  {== TRzMenuButtonEditor Class Declaration ==}
  {===========================================}

  TRzMenuButtonEditor = class( TRzBitBtnEditor )
  protected
    function GetCompRefData( Index: Integer; var CompRefClass: TComponentClass; var CompRefPropName: string;
                             var CompRefMenuHandler: TNotifyEvent ): Boolean; override;
    procedure DropDownMenuMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ): string; override;
  end;


  {================================================}
  {== TRzRapidFireButtonEditor Class Declaration ==}
  {================================================}

  TRzRapidFireButtonEditor = class( TRzDefaultEditor )
  protected
    function Button: TRzRapidFireButton;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure ScrollStyleMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ): string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {============================================}
  {== TRzSelectImageEditor Class Declaration ==}
  {============================================}

  TRzSelectImageEditor = class( TRzDefaultEditor )
  protected
    function GetCompRefData( Index: Integer; var CompRefClass: TComponentClass; var CompRefPropName: string;
                             var CompRefMenuHandler: TNotifyEvent ): Boolean; override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ): string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  TRzTextChangeType = ( tctNone, tctCaption, tctHint );

  TRzSelectImageEditDlg = class(TForm)
    BtnDone: TRzButton;
    GrpCustomImages: TRzGroupBox;
    GrpStockImages: TRzGroupBox;
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
    MnuStock: TPopupMenu;
    MnuEditStockHint: TMenuItem;
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
    BtnClearImage: TRzToolbarButton;
    ImgGroove: TImage;
    ImgSpacer: TImage;
    BtnDBFirst: TRzToolbarButton;
    ChkSetHint: TRzCheckBox;
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
    ChkAddDisabled: TRzCheckBox;
    GrpImageList: TRzGroupBox;
    LstImageList: TRzListBox;
    SbxCustom: TScrollBox;
    MnuCustomImages: TPopupMenu;
    AddCustomImage1: TMenuItem;
    DlgOpenPicture: TOpenPictureDialog;
    LblNoImageListTitle: TRzLabel;
    LblNoImageListMsg: TRzLabel;
    BtnViewList: TRzToolbarButton;
    BtnViewDetails: TRzToolbarButton;
    BtnDBInsert: TRzToolbarButton;
    BtnDBDelete: TRzToolbarButton;
    MnuCustom: TPopupMenu;
    MnuAdd: TMenuItem;
    MnuSep1: TMenuItem;
    MnuEditCustomHint: TMenuItem;
    MnuDelete: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ToolbarButtonClick(Sender: TObject);
    procedure MnuAddClick(Sender: TObject);
    procedure MnuDeleteClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MnuEditStockHintClick(Sender: TObject);
    procedure MnuEditCustomHintClick(Sender: TObject);
    procedure SbxCustomResize(Sender: TObject);
    procedure LstImageListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure LstImageListClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FObject: TPersistent;
    FImageList: TCustomImageList;
    FHasImageListProp: Boolean;
    FHasGlyphProp: Boolean;
    FIsToolButton: Boolean;
    FCustomImages: TStringList;

    procedure RearrangeCustomButtons;
    function CreateCustomButton( const S: string ): TRzToolbarButton;
    function GetFileNameFromString( const S: string ): string;
    function GetHintFromString( const S: string ): string;

    procedure AddImageToImgList( ImageList: TCustomImageList; Glyph: TBitmap );
    procedure HandleTextChange( Sender: TObject );

    function GetObjectImageIndex: Integer;
    procedure SetObjectImageIndex( Value: Integer );
    procedure SetObjectDisabledIndex( Value: Integer );
    procedure SetObjectCaption( const Value: string );
    procedure SetObjectHint( const Value: string );
    procedure SetObjectGlyph( Glyph: TBitmap );
    procedure SetObjectNumGlyphs( Value: Integer );
  public
    FPropertyEditor: TRzCustomImageIndexProperty;
    FEditingProperty: Boolean;
    SelectedBtn: TRzToolbarButton;
    CompOwner: TComponent;
    TextChangeType: TRzTextChangeType;
    procedure Reposition;
    procedure UpdateControls;
    procedure SetPropertyEditor( PropEditor: TRzCustomImageIndexProperty );

    procedure SetObject( Value: TPersistent; EditingProperty: Boolean );
  end;


implementation

{$R *.dfm}

uses
  RzCommon,
  IniFiles,
  TypInfo,
  Registry,
  RzAnimtr,
  RzGroupBar,
  RzPathBar,
  RzTabs,
  RzTray;

const
  CustomButtonsSection = 'CustomButtons';
  StockHintsSection = 'StockHints';
  ToolbarButtonsSection = 'ToolbarButtons';
  SelectImageSection = 'SelectImage';



{=========================================}
{== TRzCustomImageIndexProperty Methods ==}
{=========================================}


function TRzCustomImageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes +
            [ paValueList, paDialog ] - [ paSortList, paMultiSelect, paAutoUpdate, paSubProperties, paReadOnly ];
end;


procedure TRzCustomImageIndexProperty.GetValues( Proc: TGetStrProc );
var
  I: Integer;
  Images: TCustomImageList;
begin
  Images := GetImageList;
  if Images <> nil then
  begin
    for I := 0 to Images.Count - 1 do
     Proc( IntToStr( I ) );
  end;
end;


function TRzCustomImageIndexProperty.GetImageHeight( Images: TCustomImageList; ACanvas: TCanvas ): Integer;
begin
  Result := Min( Max( ACanvas.TextHeight('Wg'), Images.Height ), 50 );
end;


function TRzCustomImageIndexProperty.GetImageWidth( Images: TCustomImageList; ACanvas: TCanvas ): Integer;
var
  H: Integer;
begin
  H := GetImageHeight( Images, ACanvas );
  if H < Images.Height then
    Result := Round( Images.Width * ( H / Images.Height ) )
  else
    Result := Images.Width;
end;


procedure TRzCustomImageIndexProperty.ListMeasureHeight( const Value: string; ACanvas: TCanvas; var AHeight: Integer );
var
  Images: TCustomImageList;
begin
  Images := GetImageList;
  if Images <> nil then
    AHeight := Max( ACanvas.TextHeight('Wg'), Images.Height + 4 );
end;


procedure TRzCustomImageIndexProperty.ListMeasureWidth( const Value: string; ACanvas: TCanvas; var AWidth: Integer );
var
  Images: TCustomImageList;
begin
  Images := GetImageList;
  if Images <> nil then
    AWidth := AWidth + Images.Width + 4;
end;


procedure TRzCustomImageIndexProperty.ListDrawValue( const Value: string; ACanvas: TCanvas; const ARect: TRect;
                                                     ASelected: Boolean );
var
  R: TRect;
  Images: TCustomImageList;
begin
  R := ARect;
  R.Right := R.Left + ( R.Bottom - R.Top );
  try
    ACanvas.FillRect( R );
    Images := GetImageList;
    if Images <> nil then
      Images.Draw( ACanvas, R.Left + 2, R.Top + 2, StrToInt( Value ), True );
  finally
    DefaultPropertyListDrawValue( Value, ACanvas, Rect( R.Right, ARect.Top, ARect.Right, ARect.Bottom ), ASelected );
  end;
end;


procedure TRzCustomImageIndexProperty.Edit;
var
  Dlg: TRzSelectImageEditDlg;
  PropObj: TPersistent;
  Component: TComponent;
  OwnerName: string;
begin
  PropObj := GetComponent( 0 );

  if PropObj is TComponent then
    Component := TComponent( PropObj )
  else
    Component := nil;

  Dlg := TRzSelectImageEditDlg.Create( Application );
  try
    if ( Component <> nil ) and ( Component.Owner <> nil ) then
      OwnerName := Component.Owner.Name + '.'
    else
      OwnerName := '';

    if Component <> nil then
      Dlg.Caption := OwnerName + Component.Name + '.' + GetName + Dlg.Caption
    else
      Dlg.Caption := GetName + Dlg.Caption;

    Dlg.CompOwner := Designer.GetRoot;

    Dlg.SetPropertyEditor( Self );

    Dlg.SetObject( PropObj, True );
    Dlg.UpdateControls;
    Dlg.Reposition;
    Dlg.ShowModal;
    Designer.Modified;
  finally
    Dlg.Free;
  end;

end;


{===================================}
{== TRzImageIndexProperty Methods ==}
{===================================}

function TRzImageIndexProperty.GetImageList: TCustomImageList;
begin
  Result := GetObjectProp( GetComponent( 0 ), 'Images' ) as TCustomImageList;
end;



{=============================================}
{== TRzToolButtonImageIndexProperty Methods ==}
{=============================================}

function TRzToolButtonImageIndexProperty.GetImageList: TCustomImageList;
begin
  // ImageList is a function in the TRzToolButton class that will return the ImageList associated with the ToolButton's
  // Images property, or the ImageList associated with the Toolbar's Images property.

  Result := ( GetComponent( 0 ) as TRzToolButton ).ImageList;
end;


{===========================================}
{== TRzTabSheetImageIndexProperty Methods ==}
{===========================================}

function TRzTabSheetImageIndexProperty.GetImageList: TCustomImageList;
begin
  Result := ( GetComponent( 0 ) as TRzTabSheet ).PageControl.Images;
end;



{===============================================}
{== TRzGroupCaptionImageIndexProperty Methods ==}
{===============================================}

function TRzGroupCaptionImageIndexProperty.GetImageList: TCustomImageList;
begin
  // CaptionImageList is a function in the TRzGroup class that will return the ImageList associated with the Group's
  // SmallImages or LargeImages property, or the ImageList associated with the GroupBar's SmallImages or LargeImages
  // according to the CaptionStyle property.

  Result := ( GetComponent( 0 ) as TRzGroup ).CaptionImageList;
end;


{============================================}
{== TRzGroupItemImageIndexProperty Methods ==}
{============================================}

function TRzGroupItemImageIndexProperty.GetImageList: TCustomImageList;
var
  G: TRzGroup;
begin
  // SmallImageList is a function in the TRzGroup class that will return the ImageList associated with the Group's
  // SmallImages property, or the ImageList associated with the GroupBar's SmallImages property.

  G := ( GetComponent( 0 ) as TRzGroupItem ).Group;
  if G.ItemStyle = isSmall then
    Result := G.SmallImageList
  else
    Result := G.LargeImageList;
end;


{===========================================}
{== TRzPathItemImageIndexProperty Methods ==}
{===========================================}

function TRzPathItemImageIndexProperty.GetImageList: TCustomImageList;
begin
  Result := ( GetComponent( 0 ) as TRzPathItem ).PathBar.Images;
end;


{=======================================================}
{== TRzGroupTemplateCaptionImageIndexProperty Methods ==}
{=======================================================}

function TRzGroupTemplateCaptionImageIndexProperty.GetImageList: TCustomImageList;
begin
  // SmallImageList is a function in the TRzGroup class that will return the ImageList associated with the Group's
  // SmallImages property, or the ImageList associated with the GroupBar's SmallImages property.

  Result := ( GetComponent( 0 ) as TRzGroupTemplate ).PreviewOptions.SmallImages;
end;


{====================================================}
{== TRzGroupTemplateItemImageIndexProperty Methods ==}
{====================================================}

function TRzGroupTemplateItemImageIndexProperty.GetImageList: TCustomImageList;
var
  T: TRzGroupTemplate;
begin
  // SmallImageList is a function in the TRzGroup class that will return the ImageList associated with the Group's
  // SmallImages property, or the ImageList associated with the GroupBar's SmallImages property.

  T := ( GetComponent( 0 ) as TRzGroupTemplateItem ).Template;
  if T.PreviewOptions.ItemStyle = isSmall then
    Result := T.PreviewOptions.SmallImages
  else
    Result := T.PreviewOptions.LargeImages;
end;


{===========================================}
{== TRzAnimatorImageIndexProperty Methods ==}
{===========================================}

function TRzAnimatorImageIndexProperty.GetImageList: TCustomImageList;
begin
  Result := ( GetComponent( 0 ) as TRzAnimator ).ImageList;
end;


{======================================}
{== TRzTrayIconIndexProperty Methods ==}
{======================================}

function TRzTrayIconIndexProperty.GetImageList: TCustomImageList;
begin
  Result := ( GetComponent( 0 ) as TRzTrayIcon ).Icons;
end;



{=================================}
{== TRzToolButtonEditor Methods ==}
{=================================}

function TRzToolButtonEditor.ToolButton: TRzToolButton;
begin
  // Helper function to provide quick access to component being edited. Also makes sure Component is a TRzToolButton.
  Result := Component as TRzToolButton;
end;


function TRzToolButtonEditor.GetVerbCount: Integer;
begin
  Result := 12;
end;


function TRzToolButtonEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Select Image...';
    1: Result := 'Select Disabled Image...';
    2: Result := 'Select Hot Image...';
    3: Result := 'Select Down Image...';
    4: Result := '-';
    5: Result := 'Set ImageList';
    6: Result := 'Set DropDownMenu';
    7: Result := '-';
    8: Result := 'ToolStyle - Button';
    9: Result := 'ToolStyle - DropDown';
    10: Result := 'Show Caption';
    11: Result := 'Layout';
  end;
end;


function TRzToolButtonEditor.GetCompRefData( Index: Integer; var CompRefClass: TComponentClass;
                                             var CompRefPropName: string; var CompRefMenuHandler: TNotifyEvent ): Boolean;
begin
  Result := False;
  case Index of
    5:
    begin
      CompRefClass := TCustomImageList;
      CompRefPropName := 'Images';
      CompRefMenuHandler := nil;
      Result := True;
    end;

    6:
    begin
      CompRefClass := TPopupMenu;
      CompRefPropName := 'DropDownMenu';
      CompRefMenuHandler := DropDownMenuMenuHandler;
      Result := True;
    end;
  end;
end;


procedure TRzToolButtonEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );

  procedure CreateLayoutMenu( Layout: TButtonLayout; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Layout );
    NewItem.Checked := ToolButton.Layout = Layout;
    NewItem.OnClick := LayoutMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  case Index of
    8: Item.Checked := ToolButton.ToolStyle = tsButton;
    9: Item.Checked := ToolButton.ToolStyle = tsDropDown;
    10: Item.Checked := ToolButton.ShowCaption;
    11:
    begin
      CreateLayoutMenu( blGlyphLeft, 'Image Left' );
      CreateLayoutMenu( blGlyphRight, 'Image Right' );
      CreateLayoutMenu( blGlyphTop, 'Image Top' );
      CreateLayoutMenu( blGlyphBottom, 'Image Bottom' );
    end;
  end;
end;


procedure TRzToolButtonEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    0: EditPropertyByName( 'ImageIndex' );
    1: EditPropertyByName( 'DisabledIndex' );
    2: EditPropertyByName( 'HotIndex' );
    3: EditPropertyByName( 'DownIndex' );

    8:
    begin
      ToolButton.ToolStyle := tsButton;
      DesignerModified;
    end;

    9:
    begin
      ToolButton.ToolStyle := tsDropDown;
      DesignerModified;
    end;

    10:
    begin
      ToolButton.ShowCaption := not ToolButton.ShowCaption;
      DesignerModified;
    end;
  end;
end;


procedure TRzToolButtonEditor.LayoutMenuHandler( Sender: TObject );
begin
  ToolButton.Layout := TButtonLayout( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzToolButtonEditor.DropDownMenuMenuHandler( Sender: TObject );
var
  S: string;
  PM: TPopupMenu;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    PM := Designer.GetRoot.FindComponent( S ) as TPopupMenu;
    SetObjectProp( Component, 'DropDownMenu', PM );
    DesignerModified;
  end;
end;



{=============================}
{== TRzBitBtnEditor Methods ==}
{=============================}

function TRzBitBtnEditor.BitBtn: TRzBitBtn;
begin
  Result := Component as TRzBitBtn;
end;


function TRzBitBtnEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;


function TRzBitBtnEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Select Image...';
    1: Result := 'Set ImageList';
    2: Result := '-';
    3: Result := 'HotTrack Style';
    4: Result := 'XP Colors';
  end;
end;


function TRzBitBtnEditor.GetCompRefData( Index: Integer; var CompRefClass: TComponentClass;
                                         var CompRefPropName: string; var CompRefMenuHandler: TNotifyEvent ): Boolean;
begin
  Result := False;
  case Index of
    1:
    begin
      CompRefClass := TCustomImageList;
      CompRefPropName := 'Images';
      CompRefMenuHandler := nil;
      Result := True;
    end;
  end;
end;



procedure TRzBitBtnEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    3: Item.Checked := BitBtn.HotTrack;
  end;
end;


procedure TRzBitBtnEditor.ExecuteVerb( Index: Integer );
var
  Dlg: TRzSelectImageEditDlg;
  OwnerName: string;
begin
  case Index of
    0:
    begin
      Dlg := TRzSelectImageEditDlg.Create( Application );
      try
        if Component.Owner <> nil then
          OwnerName := Component.Owner.Name + '.'
        else
          OwnerName := '';
        Dlg.Caption := OwnerName + Component.Name + Dlg.Caption;
        Dlg.CompOwner := Designer.GetRoot;
        Dlg.SetObject( TControl( Component ), False );
        Dlg.UpdateControls;
        Dlg.Reposition;
        Dlg.ShowModal;
        DesignerModified;
      finally
        Dlg.Free;                         { Don't forget to free Dlg box }
      end;
    end;

    3:
    begin
      BitBtn.HotTrack := not BitBtn.HotTrack;
      DesignerModified;
    end;

    4:
    begin
      BitBtn.HotTrack := True;
      BitBtn.HotTrackColorType := htctActual;
      BitBtn.HotTrackColor := xpHotTrackColor;
      BitBtn.HighlightColor := clHighlight;
      BitBtn.Color := xpButtonFaceColor;
      BitBtn.FrameColor := xpButtonFrameColor;
      DesignerModified;
    end;
  end; { case }
end; {= TRzBitBtnEditor.ExecuteVerb =}



{=================================}
{== TRzMenuButtonEditor Methods ==}
{=================================}

function TRzMenuButtonEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;


function TRzMenuButtonEditor.GetVerb( Index: Integer ): string;
begin
  Result := inherited GetVerb( Index );

  case Index of
    5: Result := '-';
    6: Result := 'Set DropDownMenu';
  end;
end;


function TRzMenuButtonEditor.GetCompRefData( Index: Integer; var CompRefClass: TComponentClass;
                                             var CompRefPropName: string;
                                             var CompRefMenuHandler: TNotifyEvent ): Boolean;
begin
  Result := inherited GetCompRefData( Index, CompRefClass, CompRefPropName, CompRefMenuHandler );

  case Index of
    6:
    begin
      CompRefClass := TPopupMenu;
      CompRefPropName := 'DropDownMenu';
      CompRefMenuHandler := DropDownMenuMenuHandler;
      Result := True;
    end;
  end;
end;


procedure TRzMenuButtonEditor.DropDownMenuMenuHandler( Sender: TObject );
var
  S: string;
  PM: TPopupMenu;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    PM := Designer.GetRoot.FindComponent( S ) as TPopupMenu;
    SetObjectProp( Component, 'DropDownMenu', PM );
    DesignerModified;
  end;
end;



{======================================}
{== TRzRapidFireButtonEditor Methods ==}
{======================================}

function TRzRapidFireButtonEditor.Button: TRzRapidFireButton;
begin
  Result := Component as TRzRapidFireButton;
end;


function TRzRapidFireButtonEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;


function TRzRapidFireButtonEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Select Image...';
    1: Result := '-';
    2: Result := 'Scroll Style';
    3: Result := 'Flat';
    4: Result := 'Transparent';
  end;
end;


procedure TRzRapidFireButtonEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );

  procedure CreateScrollStyleMenu( Style: TRzScrollStyle; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Style );
    NewItem.Checked := Button.ScrollStyle = Style;
    NewItem.OnClick := ScrollStyleMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;
  
  case Index of
    2: // ScrollStyle
    begin
      CreateScrollStyleMenu( scsNone, 'None' );
      CreateScrollStyleMenu( scsLeft, 'Left' );
      CreateScrollStyleMenu( scsUp, 'Up' );
      CreateScrollStyleMenu( scsRight, 'Right' );
      CreateScrollStyleMenu( scsDown, 'Down' );
    end;

    3: Item.Checked := Button.Flat;
    4: Item.Checked := Button.Transparent;
  end;
end;


procedure TRzRapidFireButtonEditor.ExecuteVerb( Index: Integer );
var
  Dlg: TRzSelectImageEditDlg;
  OwnerName: string;
begin
  case Index of
    0:
    begin
      Dlg := TRzSelectImageEditDlg.Create( Application );
      try
        if Component.Owner <> nil then
          OwnerName := Component.Owner.Name + '.'
        else
          OwnerName := '';
        Dlg.Caption := OwnerName + Component.Name + Dlg.Caption;
        Dlg.CompOwner := Designer.GetRoot;
        Dlg.SetObject( TControl( Component ), False );
        Dlg.UpdateControls;
        Dlg.Reposition;
        Dlg.ShowModal;
        DesignerModified;
      finally
        Dlg.Free;                         { Don't forget to free Dlg box }
      end;
    end;

    3:
    begin
      Button.Flat := not Button.Flat;
      DesignerModified;
    end;

    4:
    begin
      Button.Transparent := not Button.Transparent;
      DesignerModified;
    end;
  end; { case }
end; {= TRzRapidFireButtonEditor.ExecuteVerb =}


procedure TRzRapidFireButtonEditor.ScrollStyleMenuHandler( Sender: TObject );
begin
  Button.ScrollStyle := TRzScrollStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


{==================================}
{== TRzSelectImageEditor Methods ==}
{==================================}

function TRzSelectImageEditor.GetVerbCount: Integer;
begin
  if IsPublishedProp( Component, 'Images' ) then
    Result := 2
  else
    Result := 1;
end;


function TRzSelectImageEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Select Image...';
    1: Result := 'Set ImageList';
  end;
end;


function TRzSelectImageEditor.GetCompRefData( Index: Integer; var CompRefClass: TComponentClass;
                                              var CompRefPropName: string;
                                              var CompRefMenuHandler: TNotifyEvent ): Boolean;
begin
  Result := False;
  if Index = 1 then
  begin
    CompRefClass := TCustomImageList;
    CompRefPropName := 'Images';
    CompRefMenuHandler := nil;
    Result := True;
  end
end;


procedure TRzSelectImageEditor.ExecuteVerb( Index: Integer );
var
  Dlg: TRzSelectImageEditDlg;
  OwnerName: string;
begin
  case Index of
    0:
    begin
      Dlg := TRzSelectImageEditDlg.Create( Application );
      try
        if Component.Owner <> nil then
          OwnerName := Component.Owner.Name + '.'
        else
          OwnerName := '';
        Dlg.Caption := OwnerName + Component.Name + Dlg.Caption;
        Dlg.CompOwner := Designer.GetRoot;
        Dlg.SetObject( TControl( Component ), False );
        Dlg.UpdateControls;
        Dlg.Reposition;
        Dlg.ShowModal;
        DesignerModified;
      finally
        Dlg.Free;                         { Don't forget to free Dlg box }
      end;
    end;

  end; { case }
end; {= TRzSelectImageEditor.ExecuteVerb =}



{===================================}
{== TRzSelectImageEditDlg Methods ==}
{===================================}

procedure TRzSelectImageEditDlg.FormCreate(Sender: TObject);
var
  BtnCount: Integer;
  I: Integer;
  S: string;
  R: TRegIniFile;
begin
  PopupMode := pmAuto;
  Position := poDesigned;

  Icon.Handle := LoadIcon( HInstance, 'RZDESIGNEDITORS_SELECT_IMAGE_ICON' );

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

    ChkAddDisabled.Checked := R.ReadBool( SelectImageSection, 'AddDisabled', True );

    // Read in Set Hint Setting
    ChkSetHint.Checked := R.ReadBool( StockHintsSection, 'SetHint', True );

    Width := R.ReadInteger( SelectImageSection, 'Width', 665 );
    Height := R.ReadInteger( SelectImageSection, 'Height', 457 );

  finally
    R.Free;
  end;
end; {= TRzSelectImageEditDlg.FormCreate =}


procedure TRzSelectImageEditDlg.FormDestroy(Sender: TObject);
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

    // Save Set Hint Setting
    R.WriteBool( StockHintsSection, 'SetHint', ChkSetHint.Checked );
    R.WriteBool( SelectImageSection, 'AddDisabled', ChkAddDisabled.Checked );
    R.WriteInteger( SelectImageSection, 'Height', Height );
    if GrpImageList.Visible then
      R.WriteInteger( SelectImageSection, 'Width', Width )
    else
      R.WriteInteger( SelectImageSection, 'Width', Width + GrpImageList.Width + 20 );
  finally
    R.Free;
  end;

  FCustomImages.Free;
end; {= TRzSelectImageEditDlg.FormDestroy =}


procedure TRzSelectImageEditDlg.SetPropertyEditor( PropEditor: TRzCustomImageIndexProperty );
begin
  FPropertyEditor := PropEditor;
  FEditingProperty := True;
end;


procedure TRzSelectImageEditDlg.SetObject( Value: TPersistent; EditingProperty: Boolean );
begin
  FObject := Value;
  FEditingProperty := EditingProperty;

  if FObject <> nil then
  begin
    if FEditingProperty then
    begin
      FIsToolButton := False;
      FHasImageListProp := True;
      FHasGlyphProp := False;
      FImageList := TRzCustomImageIndexProperty( FPropertyEditor ).GetImageList;
    end
    else if FObject is TCustomImageList then
    begin
      TextChangeType := tctNone;
      FHasGlyphProp := False;
      FHasImageListProp := True;
      FImageList := TCustomImageList( FObject );
      BtnClearImage.Visible := False;
    end
    else
    begin
      FIsToolButton := FObject is TRzToolButton;

      if FIsToolButton then
      begin
        TextChangeType := tctCaption;
        FHasImageListProp := True;
        FHasGlyphProp := False;
        FImageList := TRzToolButton( FObject ).ImageList;
      end
      else
      begin
        // Control is not a TRzToolButton.  Therefore, need to determine if it supports an ImageList or a Glyph property
        FHasImageListProp := IsPublishedProp( FObject, 'Images' );
        FHasGlyphProp := IsPublishedProp( FObject, 'Glyph' );

        if FHasImageListProp then
          FImageList := GetObjectProp( FObject, 'Images' ) as TCustomImageList
        else
          FImageList := nil;

        if FObject is TRzToolbarButton then
          TextChangeType := tctHint
        else if FObject is TRzBitBtn then
          TextChangeType := tctCaption;
      end;
    end;
  end
  else
  begin
    // FObject is nil, not much we can do then
    FIsToolButton := False;
    FHasImageListProp := False;
    FHasGlyphProp := False;
    FImageList := nil;
  end;
end; {= TRzSelectImageEditDlg.SetObject =}


function TRzSelectImageEditDlg.GetObjectImageIndex: Integer;
begin
  if IsPublishedProp( FObject, 'ImageIndex' ) then
    Result := GetOrdProp( FObject, 'ImageIndex' )
  else
    Result := -1;
end;


procedure TRzSelectImageEditDlg.SetObjectImageIndex( Value: Integer );
begin
  if FEditingProperty then
    TRzCustomImageIndexProperty( FPropertyEditor ).SetOrdValue( Value )
  else if IsPublishedProp( FObject, 'ImageIndex' ) then
    SetOrdProp( FObject, 'ImageIndex', Value );
end;


procedure TRzSelectImageEditDlg.SetObjectDisabledIndex( Value: Integer );
begin
  if IsPublishedProp( FObject, 'DisabledIndex' ) then
    SetOrdProp( FObject, 'DisabledIndex', Value );
end;


procedure TRzSelectImageEditDlg.SetObjectCaption( const Value: string );
begin
  if IsPublishedProp( FObject, 'Caption' ) then
    SetStrProp( FObject, 'Caption', Value );
end;


procedure TRzSelectImageEditDlg.SetObjectHint( const Value: string );
begin
  if IsPublishedProp( FObject, 'Hint' ) then
    SetStrProp( FObject, 'Hint', Value );
end;


procedure TRzSelectImageEditDlg.SetObjectGlyph( Glyph: TBitmap );
begin
  if IsPublishedProp( FObject, 'Glyph' ) then
  begin
    SetObjectProp( FObject, 'Glyph', Glyph );
    if Glyph.Width <> 0 then
      SetObjectNumGlyphs( Glyph.Width div Glyph.Height );
  end;
end;


procedure TRzSelectImageEditDlg.SetObjectNumGlyphs( Value: Integer );
begin
  if IsPublishedProp( FObject, 'NumGlyphs' ) then
    SetOrdProp( FObject, 'NumGlyphs', Value );
end;



procedure TRzSelectImageEditDlg.UpdateControls;
var
  I: Integer;
begin
  if FHasImageListProp then
  begin
    Constraints.MinWidth := 665;
    ChkAddDisabled.Visible := FImageList <> nil;

    if FImageList = nil then
    begin
      if FHasGlyphProp then
      begin
        // Allow user to select an image for the Glyph property;
        Constraints.MinWidth := 490;
        GrpImageList.Visible := False;
        ChkAddDisabled.Visible := False;
        Width := Width - GrpImageList.Width - 20;
      end
      else
      begin
        GrpStockImages.Enabled := False;
        GrpCustomImages.Enabled := False;
        for I := 0 to FCustomImages.Count - 1 do
          TRzToolbarButton( FCustomImages.Objects[ I ] ).Enabled := False;
        GrpImageList.Enabled := False;
        LstImageList.Color := clBtnFace;
        LblNoImageListTitle.Visible := True;
        LblNoImageListTitle.Enabled := True;
        LblNoImageListTitle.Height := 120;
        LblNoImageListTitle.BringToFront;
        if FObject <> nil then
        begin
          if FObject is TComponent then
            LblNoImageListMsg.Caption := Format( LblNoImageListMsg.Caption, [ TComponent( FObject ).Name ] )
          else
            LblNoImageListMsg.Caption := Format( LblNoImageListMsg.Caption, [ FObject.ClassName + ' instance' ] )
        end;
        LblNoImageListMsg.Visible := True;
        LblNoImageListMsg.Height := 190;
        LblNoImageListMsg.Enabled := True;
        LblNoImageListMsg.BringToFront;
      end;
    end
    else
    begin
      GrpImageList.Caption := Format( '%s Images', [ FImageList.Name ] );
      for I := 0 to FImageList.Count - 1 do
        LstImageList.Items.Add( IntToStr( I ) );
      if FEditingProperty then
        LstImageList.ItemIndex := TRzCustomImageIndexProperty( FPropertyEditor ).GetOrdValue
      else
        LstImageList.ItemIndex := GetObjectImageIndex;
    end;
  end
  else
  begin
    Constraints.MinWidth := 490;
    GrpImageList.Visible := False;
    ChkAddDisabled.Visible := False;
    Width := Width - GrpImageList.Width - 20;
  end;

  case TextChangeType of
    tctNone:
      ChkSetHint.Visible := False;

    tctCaption:
      ChkSetHint.Caption := 'Set Button Caption';

    tctHint:
      ChkSetHint.Caption := 'Set Button Hint';
  end;
end; {= TRzSelectImageEditDlg.UpdateControls =}


procedure TRzSelectImageEditDlg.Reposition;
var
  T, L: Integer;
  R, DeskRect: TRect;
  P: TPoint;
begin
  if FObject <> nil then
  begin
    if FObject is TControl then
    begin
      R := TControl( FObject ).BoundsRect;
      P := Point( R.Left, R.Top + 25 );

      P := GetParentForm( TControl( FObject ) ).ClientToScreen( P );

      T := P.Y + 5;
      L := P.X + 5;
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
    end
    else if FObject is TComponent then
    begin
      Position := poScreenCenter;
    end;
  end;
end;


procedure TRzSelectImageEditDlg.AddImageToImgList( ImageList: TCustomImageList; Glyph: TBitmap );
var
  ImageIndex, DisabledIndex: Integer;
  DM: TDataModule;
  I: Integer;
  F: TForm;
begin
  if ( ImageList <> nil ) and ( Glyph <> nil ) then
  begin
    AddImageToImageList( ImageList, Glyph, ChkAddDisabled.Checked, ImageIndex, DisabledIndex );

    if ( ImageList.Owner <> nil ) and ( ImageList.Owner is TDataModule ) then
    begin
      // If the ImageList is on a DataModule, we must get to the DataModule's
      // designer and tell it that is has been modified.
      
      DM := TDataModule( ImageList.Owner );
      
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

    SetObjectImageIndex( ImageIndex );

    LstImageList.Items.Add( IntToStr( ImageIndex ) );
    if DisabledIndex <> -1 then
    begin
      if not FEditingProperty then
        SetObjectDisabledIndex( DisabledIndex );
      LstImageList.Items.Add( IntToStr( DisabledIndex ) );
    end;

    LstImageList.ItemIndex := ImageIndex;
  end;
end;


procedure TRzSelectImageEditDlg.ToolbarButtonClick(Sender: TObject);
begin
  if CompOwner <> nil then
  begin
    SelectedBtn := TRzToolbarButton( Sender );

    if ( FImageList <> nil ) and not FEditingProperty then
    begin
      if Sender = BtnClearImage then
      begin
        SetObjectImageIndex( -1 );
        SetObjectDisabledIndex( -1 );
      end
      else
      begin
        AddImageToImgList( FImageList, SelectedBtn.Glyph );
      end;

      HandleTextChange( Sender );
    end
    else if FHasGlyphProp then
    begin
      SetObjectGlyph( SelectedBtn.Glyph );
      HandleTextChange( Sender );
    end
    else if FEditingProperty then
    begin
      if Sender = BtnClearImage then
      begin
        TRzCustomImageIndexProperty( FPropertyEditor ).SetOrdValue( -1 );
      end
      else
      begin
        AddImageToImgList( FImageList, SelectedBtn.Glyph );
        TRzCustomImageIndexProperty( FPropertyEditor ).SetOrdValue( LstImageList.ItemIndex );
      end;
    end;
  end;
end; {= TRzSelectImageEditDlg.ToolbarButtonClick =}


procedure TRzSelectImageEditDlg.HandleTextChange( Sender: TObject );
begin
  if ChkSetHint.Checked then
  begin
    if TextChangeType = tctCaption then
    begin
      if Sender = BtnClearImage then
        SetObjectCaption( '' )
      else
        SetObjectCaption( SelectedBtn.Hint );
    end
    else if TextChangeType = tctHint then
    begin
      if Sender = BtnClearImage then
        SetObjectHint( '' )
      else
        SetObjectHint( SelectedBtn.Hint );
    end;
  end;
end;


procedure TRzSelectImageEditDlg.RearrangeCustomButtons;
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


function TRzSelectImageEditDlg.CreateCustomButton( const S: string ): TRzToolbarButton;
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


function TRzSelectImageEditDlg.GetFileNameFromString( const S: string ): string;
begin
  Result := Copy( S, Pos( '|', S ) + 1, 255 );
end;


function TRzSelectImageEditDlg.GetHintFromString( const S: string ): string;
begin
  Result := Copy( S, 1, Pos( '|', S ) - 1 );
end;


procedure TRzSelectImageEditDlg.MnuAddClick(Sender: TObject);
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


procedure TRzSelectImageEditDlg.MnuDeleteClick(Sender: TObject);
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


procedure TRzSelectImageEditDlg.MnuEditCustomHintClick(Sender: TObject);
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


procedure TRzSelectImageEditDlg.MnuEditStockHintClick(Sender: TObject);
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


procedure TRzSelectImageEditDlg.SbxCustomResize(Sender: TObject);
begin
  RearrangeCustomButtons;
end;

procedure TRzSelectImageEditDlg.LstImageListDrawItem( Control: TWinControl; Index: Integer; Rect: TRect;
                                                      State: TOwnerDrawState );
var
  TextOffset: Integer;
begin
  LstImageList.Canvas.FillRect( Rect );   // Clear area for icon and text

  if FImageList <> nil then
    FImageList.Draw( LstImageList.Canvas, Rect.Left - 22, Rect.Top + 2, Index );

//  Inc( Rect.Left, 26 );
  TextOffset := ( ( Rect.Bottom - Rect.Top ) - LstImageList.Canvas.TextHeight( 'Pp' ) ) div 2;
  LstImageList.Canvas.TextRect( Rect, Rect.Left + 2, Rect.Top + TextOffset, IntToStr( Index ) );
end; {= TRzSelectImageEditDlg.LstCustomBmpsDrawItem =}


procedure TRzSelectImageEditDlg.LstImageListClick( Sender: TObject );
begin
  SetObjectImageIndex( LstImageList.ItemIndex );
end;


procedure TRzSelectImageEditDlg.FormResize(Sender: TObject);
begin
  if GrpImageList.Visible then
  begin
    GrpImageList.Height := ClientHeight - 14;
    GrpStockImages.Width := ClientWidth - 191;
    GrpCustomImages.Width := ClientWidth - 191;
  end
  else
  begin
    GrpStockImages.Left := 8;
    GrpStockImages.Width := ClientWidth - 16;
    GrpCustomImages.Left := 8;
    GrpCustomImages.Width := ClientWidth - 16;
    ChkSetHint.Left := 8;
    ChkAddDisabled.Left := 156;
  end;

  GrpCustomImages.Height := ClientHeight - 314;
  BtnDone.Top := ClientHeight - 31;
  BtnDone.Left := ClientWidth - 82;
  ChkSetHint.Top := ClientHeight - 37;
  ChkAddDisabled.Top := ClientHeight - 37;
end;

end.

