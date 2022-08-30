{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxIconLibraryEditor;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, cxGraphics, cxControls,
  cxContainer, cxEdit, Menus, ExtCtrls, StdCtrls, cxButtons, cxImage, cxListBox, cxLabel, cxTextEdit, cxMaskEdit,
  cxButtonEdit, dxGalleryControl, cxCheckListBox, ExtDlgs, dxGallery, cxClasses, cxGeometry, dxGDIPlusClasses,
  dxIconLibraryEditorHelpers, ComCtrls, dxCore, dxIconLibrary, ImgList, cxLookAndFeels, cxLookAndFeelPainters, cxPC,
  ActnList, cxImageList, cxCustomListBox;

type
  TdxPopulateGalleryMode = (gpmGroups, gpmSearch);

  TdxfmImagePicker = class;

  { TdxLoadImagesThread }

  TdxLoadImagesThread = class(TThread)
  private
    FImageItemList: TThreadList;
    FGallery: TdxGalleryControl;
    FList: TList;
    FPackageSize: Integer;

    procedure AssignPackage;
    procedure LoadImages;
    procedure LoadPackage;
  public
    constructor Create(AGalleryItemList: TThreadList; AGallery: TdxGalleryControl);

    procedure Execute; override;
  end;

  { TdxImagePickerFormHelper }

  TdxImagePickerFormHelper = class(TInterfacedObject, IdxAdvancedPictureEditor, IdxImageCollectionEditor)
  private
    FImagePicker: TdxfmImagePicker;
  protected
    function ExecuteImageCollectionEditor(AFiles: TStrings; const ASuggestedImageSize: TSize): Boolean; virtual;
    function ExecuteAdvancedPictureEditor(APicture: TPicture; AGraphicClass: TGraphicClass; const ABuildFilter: string; AImportList: TStrings): Boolean; virtual;
    function GetImagePicker: TdxfmImagePicker;

    //IdxImageCollection
    function IdxImageCollectionEditor.Execute =  ExecuteImageCollectionEditor;
    //IdxAdvancedPictureEditor
    function IdxAdvancedPictureEditor.Execute = ExecuteAdvancedPictureEditor;

    property ImagePicker: TdxfmImagePicker read GetImagePicker;
  public
    destructor Destroy; override;
  end;

  { TdxCustomPopulateHelper }

  TdxCustomPopulateHelper = class
  private
    FImagePicker: TdxfmImagePicker;

    function GetCategoryList: TcxCheckListBox;
    function GetCollectionList: TcxCheckListBox;
    function GetFindEditor: TcxButtonEdit;
    function GetGallery: TdxGalleryControl;
    function GetGalleryGroupHidden: TdxGalleryControlGroup;
    function GetGalleryGroupSearch: TdxGalleryControlGroup;
    function GetIconLibrary: TdxIconLibrary;
    function GetPopulateGalleryMode: TdxPopulateGalleryMode;
    function GetSizeList: TcxCheckListBox;
    function IsListBoxCheckedByText(ACheckListBox: TcxCheckListBox; const AText: string): Boolean;
    procedure SetGalleryGroupHidden(AValue: TdxGalleryControlGroup);
    procedure SetGalleryGroupSearch(AValue: TdxGalleryControlGroup);
  protected
    property CategoryList: TcxCheckListBox read GetCategoryList;
    property CollectionList: TcxCheckListBox read GetCollectionList;
    property FindEditor: TcxButtonEdit read GetFindEditor;
    property Gallery: TdxGalleryControl read GetGallery;
    property GalleryGroupHidden: TdxGalleryControlGroup read GetGalleryGroupHidden write SetGalleryGroupHidden;
    property GalleryGroupSearch: TdxGalleryControlGroup read GetGalleryGroupSearch write SetGalleryGroupSearch;
    property IconLibrary: TdxIconLibrary read GetIconLibrary;
    property PopulateGalleryMode: TdxPopulateGalleryMode read GetPopulateGalleryMode;
    property SizeList: TcxCheckListBox read GetSizeList;
  public
    constructor Create(AImagePicker: TdxfmImagePicker); virtual;

    procedure Populate; virtual; abstract;
  end;

  { TdxPopulateContentHelper }

  TdxPopulateContentHelper = class(TdxCustomPopulateHelper)
  private
    function AddCheckListBoxItem(ACheckListBox: TcxCheckListBox; const AText: string; AChecked: Boolean): Boolean;
    function AddGalleryGroup(const ACaption: string; AIndex: Integer; AVisible: Boolean): TdxGalleryControlGroup;
    procedure AddGalleryGroups(ACollectionItem: TdxIconLibrarySet);
    procedure AddGalleryItem(AImageItem: TdxIconLibraryImage);
    procedure AddGalleryItems(ACategoryItem: TdxIconLibraryCategory);
    function GetIndexForGalleryGroup(const ANameItem: string): Integer;
  public
    procedure Populate; override;
  end;

  {TdxPopulateGalleryHelper}

  TdxPopulateGalleryHelper = class(TdxCustomPopulateHelper)
  private
    FCurrentGalleryGroup: TdxGalleryControlGroup;
    FVisibleCurrentCollection: Boolean;
    FMaxSize: TSize;

    FImageItemList: TThreadList;
    FLoadImagesThread: TdxLoadImagesThread;

    procedure StartLoadImagesThread;
    procedure StopLoadImagesThread;

    procedure DestroyGalleryItems;
    function GetGalleryItem(AIconLibraryImage: TdxIconLibraryImage): TdxGalleryControlItem;
    function GetIndexForItem(AGroup: TdxGalleryControlGroup; const ANameItem: string): Integer;
    function IsImageVisible(AImageItem: TdxIconLibraryImage): Boolean;
    procedure PopulateGalleryGroups(ACollectionItem: TdxIconLibrarySet);
    procedure PopulateGalleryImages(ACategoryItem: TdxIconLibraryCategory);
  public
    constructor Create(AImagePicker: TdxfmImagePicker); override;
    destructor Destroy; Override;

    procedure Populate; override;
  end;

  { TdxfmImagePicker }

  TdxfmImagePicker = class(TForm)
    actClear: TAction;
    actF3: TAction;
    ActionList: TActionList;
    actLoad: TAction;
    actSave: TAction;
    beFind: TcxButtonEdit;
    btnCancel: TcxButton;
    btnClear: TcxButton;
    btnImport: TcxButton;
    btnLoad: TcxButton;
    btnOk: TcxButton;
    btnSave: TcxButton;
    clbCategories: TcxCheckListBox;
    clbCollection: TcxCheckListBox;
    clbSize: TcxCheckListBox;
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    EditStyleController: TcxEditStyleController;
    gcIcons: TdxGalleryControl;
    ilImages: TcxImageList;
    ImagePaintBox: TPaintBox;
    miCheckSelected: TMenuItem;
    miClear: TMenuItem;
    miImageCopy: TMenuItem;
    miImageCut: TMenuItem;
    miImagePaste: TMenuItem;
    miLine1: TMenuItem;
    miLine2: TMenuItem;
    miLoad: TMenuItem;
    miSave: TMenuItem;
    miSelectAll: TMenuItem;
    miSelectAllinThisGroup: TMenuItem;
    miSelectNone: TMenuItem;
    miUncheckSelected: TMenuItem;
    miUnselectAllinThisGroup: TMenuItem;
    OpenDialog: TOpenPictureDialog;
    pcMain: TcxPageControl;
    pmImport: TPopupMenu;
    pmPictureEditor: TPopupMenu;
    pmSelection: TPopupMenu;
    pnlFind: TPanel;
    pnlIconLibrary: TPanel;
    pnlIconLibraryButtons: TPanel;
    pnlIconLibraryOptions: TPanel;
    pnlImagePaintBox: TPanel;
    pnlPictureEditorButtons: TPanel;
    SaveDialog: TSavePictureDialog;
    tsDXImageGallery: TcxTabSheet;
    tsPictureEditor: TcxTabSheet;

    procedure actClearExecute(Sender: TObject);
    procedure actClearUpdate(Sender: TObject);
    procedure actF3Execute(Sender: TObject);
    procedure actLoadExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure beFindPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure beFindPropertiesChange(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure clbCategoriesClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
    procedure clbCollectionClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure gcIconsDblClick(Sender: TObject);
    procedure ImagePaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImagePaintBoxPaint(Sender: TObject);
    procedure miImageCopyClick(Sender: TObject);
    procedure miImagePasteClick(Sender: TObject);
    procedure miSelectAllinThisGroupClick(Sender: TObject);
    procedure miSelectClick(Sender: TObject);
    procedure miUncheckSelectedClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure pmPictureEditorPopup(Sender: TObject);
    procedure pmSelectionPopup(Sender: TObject);
  private
    FGalleryGroupHidden: TdxGalleryControlGroup;
    FGalleryGroupSearch: TdxGalleryControlGroup;
    FImportList: TStrings;
    FPicture: TPicture;
    FPopulateGalleryHelper: TdxPopulateGalleryHelper;
    FPopulateGalleryLockCount: Integer;
    FPopulateGalleryMode: TdxPopulateGalleryMode;
    FSelectedGroup: TdxGalleryControlGroup;

    procedure DrawPicture(ACanvas: TCanvas; R: TRect);
    procedure PopulateContent;
    procedure PopulateGallery;
    procedure SetImportList(AValue: TStrings);
    procedure UpdateGalleryItemsSelection(ASelect: Boolean);
    procedure ImportPictureClick(Sender: TObject);
    procedure PictureChangeHandler(Sender: TObject);
  protected
    function ExecuteImageCollectionEditor(AFiles: TStrings; const ASuggestedImageSize: TSize): Boolean; virtual;
    function ExecuteAdvancedPictureEditor(APicture: TPicture; AGraphicClass: TGraphicClass; const ABuildFilter: string; AImportList: TStrings): Boolean; virtual;
    function GetGalleryItem(AIconLibraryImage: TdxIconLibraryImage): TdxGalleryControlItem;
    function GetIconLibraryImage(AGalleryControlItem: TdxGalleryControlItem): TdxIconLibraryImage;

    property ImportList: TStrings read FImportList write SetImportList;
    property PopulateGalleryMode: TdxPopulateGalleryMode read FPopulateGalleryMode;
    property GalleryGroupSearch: TdxGalleryControlGroup read FGalleryGroupSearch write FGalleryGroupSearch;
    property GalleryGroupHidden: TdxGalleryControlGroup read FGalleryGroupHidden write FGalleryGroupHidden;
  end;

procedure dxExecuteImagePicker(APicture: TPicture; AImportList: TStrings = nil);

var
  dxIconLibrary: TdxIconLibrary;

implementation

{$R *.dfm}

uses Math, Clipbrd;

procedure dxExecuteImagePicker(APicture: TPicture; AImportList: TStrings = nil);
var
  ADialog: TdxfmImagePicker;
begin
  ADialog := TdxfmImagePicker.Create(nil);
  try
    ADialog.ExecuteAdvancedPictureEditor(APicture, TGraphic, '', AImportList);
  finally
    ADialog.Free;
  end;
end;

procedure CreateIconLibrary;
begin
  dxIconLibrary := TdxIconLibrary.Create('');
  dxIconLibrary.Populate;
end;

procedure DestroyIconLibrary;
begin
  FreeAndNil(dxIconLibrary);
end;

{ TdxLoadImagesThread }

constructor TdxLoadImagesThread.Create(AGalleryItemList: TThreadList; AGallery: TdxGalleryControl);
begin
  inherited Create(True);
  FGallery := AGallery;
  FImageItemList := AGalleryItemList;
end;

procedure TdxLoadImagesThread.Execute;
begin
  inherited;
  LoadImages;
end;

procedure TdxLoadImagesThread.AssignPackage;
var
  I: Integer;
  AIconLibraryImage: TdxIconLibraryImage;
begin
  FGallery.BeginUpdate;
  try
    for I := 0 to Min(FPackageSize, FList.Count - 1) do
    begin
      if Terminated then
        Break;
      AIconLibraryImage := TdxIconLibraryImage(FList[0]);
      TdxGalleryControlItem(AIconLibraryImage.Tag).Glyph.Assign(AIconLibraryImage.Image);
      AIconLibraryImage.Image.Clear;
      FList.Delete(0);
    end;
  finally
    FGallery.EndUpdate;
  end;
end;

procedure TdxLoadImagesThread.LoadImages;
begin
  FList := FImageItemList.LockList;
  try
    FPackageSize := Trunc(FList.Count / 10);
    while FList.Count > 0 do
    begin
      if Terminated then
        Break;
      LoadPackage;
      Synchronize(AssignPackage);
    end;
  finally
    FImageItemList.UnlockList;
  end;
end;

procedure TdxLoadImagesThread.LoadPackage;
var
  I: Integer;
begin
  for I := 0 to Min(FPackageSize, FList.Count - 1) do
  begin
    if Terminated then
      Break;
    TdxIconLibraryImage(FList[I]).LoadFromFile;
  end;
end;

{ TdxImagePickerFormHelper }

destructor TdxImagePickerFormHelper.Destroy;
begin
  FreeAndNil(FImagePicker);
  inherited Destroy;
end;

function TdxImagePickerFormHelper.ExecuteImageCollectionEditor(AFiles: TStrings; const ASuggestedImageSize: TSize): Boolean;
begin
  Result := ImagePicker.ExecuteImageCollectionEditor(AFiles, ASuggestedImageSize);
end;

function TdxImagePickerFormHelper.ExecuteAdvancedPictureEditor(APicture: TPicture;
  AGraphicClass: TGraphicClass; const ABuildFilter: string; AImportList: TStrings): Boolean;
begin
  Result := ImagePicker.ExecuteAdvancedPictureEditor(APicture, AGraphicClass, ABuildFilter, AImportList);
end;

function TdxImagePickerFormHelper.GetImagePicker: TdxfmImagePicker;
begin
  if FImagePicker = nil then
    FImagePicker := TdxfmImagePicker.Create(nil);
  Result := FImagePicker;
end;

{ TdxCustomPopulateHelper }

constructor TdxCustomPopulateHelper.Create(AImagePicker: TdxfmImagePicker);
begin
  inherited Create;
  FImagePicker := AImagePicker;
end;

function TdxCustomPopulateHelper.GetCategoryList: TcxCheckListBox;
begin
  Result := FImagePicker.clbCategories;
end;

function TdxCustomPopulateHelper.GetCollectionList: TcxCheckListBox;
begin
  Result := FImagePicker.clbCollection;
end;

function TdxCustomPopulateHelper.GetFindEditor: TcxButtonEdit;
begin
  Result := FImagePicker.beFind;
end;

function TdxCustomPopulateHelper.GetGallery: TdxGalleryControl;
begin
  Result := FImagePicker.gcIcons;
end;

function TdxCustomPopulateHelper.GetGalleryGroupHidden: TdxGalleryControlGroup;
begin
  Result := FImagePicker.GalleryGroupHidden;
end;

function TdxCustomPopulateHelper.GetGalleryGroupSearch: TdxGalleryControlGroup;
begin
  Result := FImagePicker.GalleryGroupSearch;
end;

function TdxCustomPopulateHelper.GetIconLibrary: TdxIconLibrary;
begin
  Result := dxIconLibrary;
end;

function TdxCustomPopulateHelper.GetPopulateGalleryMode: TdxPopulateGalleryMode;
begin
  Result := FImagePicker.PopulateGalleryMode;
end;

function TdxCustomPopulateHelper.GetSizeList: TcxCheckListBox;
begin
  Result := FImagePicker.clbSize;
end;

function TdxCustomPopulateHelper.IsListBoxCheckedByText(
  ACheckListBox: TcxCheckListBox; const AText: string): Boolean;
var
  AIndex: Integer;
begin
  AIndex := ACheckListBox.Items.IndexOf(AText);
  Result := (AIndex > -1) and ACheckListBox.Items[AIndex].Checked;
end;

procedure TdxCustomPopulateHelper.SetGalleryGroupHidden(AValue: TdxGalleryControlGroup);
begin
  FImagePicker.GalleryGroupHidden := AValue;
end;

procedure TdxCustomPopulateHelper.SetGalleryGroupSearch(AValue: TdxGalleryControlGroup);
begin
  FImagePicker.GalleryGroupSearch := AValue;
end;

{ TdxPopulateContentHelper }

procedure TdxPopulateContentHelper.Populate;
var
  I: Integer;
  AChecked: Boolean;
begin
  Gallery.BeginUpdate;
  CollectionList.Items.BeginUpdate;
  CategoryList.Items.BeginUpdate;
  SizeList.Items.BeginUpdate;
  try
    for I := 0 to IconLibrary.Count - 1 do
    begin
      AChecked := IconLibrary[I].DisplayName = 'Images';
      AddCheckListBoxItem(CollectionList, IconLibrary[I].DisplayName, AChecked);
      AddGalleryGroups(IconLibrary.Items[I]);
    end;
    if FImagePicker.GalleryGroupSearch = nil then
      GalleryGroupSearch := AddGalleryGroup('Search Result', Gallery.Gallery.Groups.Count, False);
    if FImagePicker.GalleryGroupHidden = nil then
      GalleryGroupHidden := AddGalleryGroup('Not Visible', Gallery.Gallery.Groups.Count, False);
  finally
    SizeList.Items.EndUpdate;
    CategoryList.Items.EndUpdate;
    CollectionList.Items.EndUpdate;
    Gallery.EndUpdate;
  end;
end;

function TdxPopulateContentHelper.AddCheckListBoxItem(
  ACheckListBox: TcxCheckListBox; const AText: string; AChecked: Boolean): Boolean;

  function IsPresentInCheckListBox: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to ACheckListBox.Items.Count - 1 do
    begin
      Result := ACheckListBox.Items[I].Text = AText;
      if Result then
        Break;
    end;
  end;

var
  ACheckListBoxItem: TcxCheckListBoxItem;
begin
  Result := not IsPresentInCheckListBox;
  if Result then
  begin
    ACheckListBoxItem := ACheckListBox.Items.Add;
    ACheckListBoxItem.Text := AText;
    ACheckListBoxItem.Checked := AChecked;
  end;
end;

function TdxPopulateContentHelper.AddGalleryGroup(
  const ACaption: string; AIndex: Integer; AVisible: Boolean): TdxGalleryControlGroup;
begin
  Result := Gallery.Gallery.Groups.Add;
  Result.Index := AIndex;
  Result.Visible := AVisible;
  Result.Caption := ACaption;
end;

procedure TdxPopulateContentHelper.AddGalleryGroups(ACollectionItem: TdxIconLibrarySet);
var
  I: Integer;
  ACategoryItem: TdxIconLibraryCategory;
begin
  for I := 0 to ACollectionItem.Count - 1 do
  begin
    ACategoryItem := ACollectionItem.Items[I];
    if AddCheckListBoxItem(CategoryList, ACategoryItem.DisplayName, True) then
      AddGalleryGroup(ACategoryItem.DisplayName, GetIndexForGalleryGroup(ACategoryItem.DisplayName), True);
    AddGalleryItems(ACategoryItem);
  end;
end;

procedure TdxPopulateContentHelper.AddGalleryItem(AImageItem: TdxIconLibraryImage);
var
  AGalleryItem: TdxGalleryControlItem;
  AChecked: Boolean;
begin
  AChecked := (AImageItem.Width = 16) and (AImageItem.Height = 16);
  AddCheckListBoxItem(SizeList, AImageItem.ImageSize, AChecked);
  if AImageItem.Tag = 0 then
  begin
    AGalleryItem := TdxGalleryControlItem.Create(nil);
    AImageItem.Tag := TdxNativeInt(AGalleryItem);
    AGalleryItem.Caption := AImageItem.DisplayName;
    AGalleryItem.Hint := AImageItem.DisplayName;
    AGalleryItem.Tag := TdxNativeInt(AImageItem);
  end;
end;

procedure TdxPopulateContentHelper.AddGalleryItems(ACategoryItem: TdxIconLibraryCategory);
var
  I: Integer;
begin
  for I := 0 to ACategoryItem.Count - 1 do
    AddGalleryItem(ACategoryItem.Items[I]);
end;

function TdxPopulateContentHelper.GetIndexForGalleryGroup(const ANameItem: string): Integer;
begin
  Result := 0;
  while (Result < Gallery.Gallery.Groups.Count) and
    (AnsiCompareStr(ANameItem, Gallery.Gallery.Groups[Result].Caption) > 0) do
      Inc(Result);
end;

{ TdxPopulateGalleryHelper }

constructor TdxPopulateGalleryHelper.Create(AImagePicker: TdxfmImagePicker);
begin
  inherited;
  FImageItemList := TThreadList.Create;
end;

destructor TdxPopulateGalleryHelper.Destroy;
begin
  StopLoadImagesThread;
  DestroyGalleryItems;
  FreeAndNil(FImageItemList);
  inherited Destroy;
end;

procedure TdxPopulateGalleryHelper.Populate;
var
  I: Integer;
  ACategoryCollection: TdxIconLibrarySet;
begin
  StopLoadImagesThread;
  Gallery.BeginUpdate;
  try
    FMaxSize := cxNullSize;
    if PopulateGalleryMode = gpmGroups then
      GalleryGroupSearch.Visible := False;

    for I := 0 to IconLibrary.Count - 1 do
    begin
      ACategoryCollection := IconLibrary[I];
      FVisibleCurrentCollection := IsListBoxCheckedByText(CollectionList, ACategoryCollection.DisplayName);
      PopulateGalleryGroups(ACategoryCollection);
    end;
    Gallery.OptionsView.Item.Image.Size.Size := FMaxSize;
  finally
    Gallery.EndUpdate;
  end;
  StartLoadImagesThread;
end;

procedure TdxPopulateGalleryHelper.StartLoadImagesThread;
begin
  FLoadImagesThread := TdxLoadImagesThread.Create(FImageItemList, Gallery);
  FLoadImagesThread.Start;
end;

procedure TdxPopulateGalleryHelper.StopLoadImagesThread;
var
  I: Integer;
  AList: TList;
begin
  if FLoadImagesThread <> nil then
  begin
    FLoadImagesThread.OnTerminate := nil;
    FLoadImagesThread.Terminate;
    FLoadImagesThread.WaitFor;
    AList := FImageItemList.LockList;
    try
      for I := 0 to AList.Count - 1 do
        if not TdxIconLibraryImage(AList[I]).Image.Empty then
          TdxIconLibraryImage(AList[I]).Image.Clear;
    finally
      FImageItemList.UnlockList;
    end;
    FImageItemList.Clear;
    FreeAndNil(FLoadImagesThread);
  end;
end;

procedure TdxPopulateGalleryHelper.DestroyGalleryItems;

  procedure DestroyItemsInGroup(ACategoryItem: TdxIconLibraryCategory);
  var
    I: Integer;
  begin
    for I := 0 to ACategoryItem.Count - 1 do
      GetGalleryItem(ACategoryItem.Items[I]).Free;
  end;

  procedure DestroyItemsInCollection(ACollectionItem: TdxIconLibrarySet);
  var
    I: Integer;
  begin
    for I := 0 to ACollectionItem.Count - 1 do
      DestroyItemsInGroup(ACollectionItem.Items[I]);
  end;

var
  I : Integer;
begin
  for I := 0 to IconLibrary.Count - 1 do
    DestroyItemsInCollection(IconLibrary.Items[I]);
end;

function TdxPopulateGalleryHelper.GetGalleryItem(
  AIconLibraryImage: TdxIconLibraryImage): TdxGalleryControlItem;
begin
  Result := FImagePicker.GetGalleryItem(AIconLibraryImage);
end;

function TdxPopulateGalleryHelper.GetIndexForItem(
  AGroup: TdxGalleryControlGroup; const ANameItem: string): Integer;
begin
  Result := 0;
  while (Result < AGroup.ItemCount) and (AnsiCompareStr(ANameItem, AGroup.Items[Result].Caption) > 0) do
    Inc(Result);
end;

function TdxPopulateGalleryHelper.IsImageVisible(AImageItem: TdxIconLibraryImage): Boolean;
begin
  Result := FVisibleCurrentCollection and IsListBoxCheckedByText(SizeList, AImageItem.ImageSize) and
    ((FindEditor.Text = '') or (Pos(AnsiLowerCase(FindEditor.Text), AnsiLowerCase(AImageItem.DisplayName)) > 0));
end;

procedure TdxPopulateGalleryHelper.PopulateGalleryGroups(ACollectionItem: TdxIconLibrarySet);
var
  I: Integer;
  ACategoryItem: TdxIconLibraryCategory;
begin
  for I := 0 to ACollectionItem.Count - 1 do
  begin
    ACategoryItem := ACollectionItem[I];
    FCurrentGalleryGroup := Gallery.Gallery.Groups[CategoryList.Items.IndexOf(ACategoryItem.DisplayName)];
    if PopulateGalleryMode = gpmSearch then
    begin
      FCurrentGalleryGroup.Visible := False;
      FCurrentGalleryGroup := GalleryGroupSearch;
    end;
    PopulateGalleryImages(ACategoryItem);
    FCurrentGalleryGroup.Visible := (PopulateGalleryMode = gpmSearch) or
      ((FCurrentGalleryGroup.ItemCount > 0) and IsListBoxCheckedByText(CategoryList, ACategoryItem.DisplayName));
  end;
end;

procedure TdxPopulateGalleryHelper.PopulateGalleryImages(ACategoryItem: TdxIconLibraryCategory);
var
  I: Integer;
  AImageItem: TdxIconLibraryImage;
  AGalleryItem: TdxGalleryControlItem;
begin
  I := 0;
  while I <= ACategoryItem.Count - 1 do
  begin
    AImageItem := ACategoryItem[I];
    AGalleryItem := GetGalleryItem(AImageItem);
    AGalleryItem.Group := GalleryGroupHidden;
    if IsImageVisible(AImageItem) and IsListBoxCheckedByText(CategoryList, ACategoryItem.DisplayName) then
    begin
      AGalleryItem.Group := FCurrentGalleryGroup;
      AGalleryItem.Index := GetIndexForItem(FCurrentGalleryGroup, AImageItem.DisplayName);
      FMaxSize := cxSizeMax(FMaxSize, Size(AImageItem.Width, AImageItem.Height));
      if AImageItem.IsFileExists then
      begin
        if AGalleryItem.Glyph.Empty then
          FImageItemList.Add(AImageItem);
      end
      else
      begin
        TdxGalleryItem(AImageItem.Tag).Collection.Remove(TdxGalleryItem(AImageItem.Tag));
        ACategoryItem.Remove(AImageItem);
      end;
    end;
    Inc(I);
  end;
end;

{ TdxfmImagePicker }

function TdxfmImagePicker.ExecuteImageCollectionEditor(AFiles: TStrings; const ASuggestedImageSize: TSize): Boolean;

  procedure AddItemInFileList(AItem: TdxGalleryControlItem);
  begin
    if AItem.Checked then
      AFiles.Add(GetIconLibraryImage(AItem).FileName);
  end;

  procedure PopulateFileListByGroup(AGroup: TdxGalleryControlGroup);
  var
    I: Integer;
  begin
    for I := 0 to AGroup.ItemCount - 1 do
      AddItemInFileList(AGroup.Items[I]);
  end;

  procedure PopulateFileList;
  var
    I: Integer;
  begin
    for I := 0 to gcIcons.Gallery.Groups.Count - 1 do
      if gcIcons.Gallery.Groups[I].Visible then
        PopulateFileListByGroup(gcIcons.Gallery.Groups[I]);
  end;

var
  I: Integer;
  ASizeString: string;
begin
  gcIcons.OptionsBehavior.ItemCheckMode := icmMultiple;
  gcIcons.PopupMenu := pmSelection;
  tsPictureEditor.TabVisible := False;
  PopulateContent;
  ASizeString := IntToStr(ASuggestedImageSize.cx) + 'x' + IntToStr(ASuggestedImageSize.cy);
  for I := 0 to clbSize.Count - 1 do
    clbSize.Items[I].Checked := (clbSize.Items[I].Text = ASizeString) or (clbSize.Items[I].Text = 'Vector') or
      cxSizeIsEmpty(ASuggestedImageSize);

  PopulateGallery;
  Result := (ShowModal = mrOk) and (gcIcons.Gallery.GetCheckedItem <> nil);
  if Result then
    PopulateFileList;
  UpdateGalleryItemsSelection(False);
end;

function TdxfmImagePicker.ExecuteAdvancedPictureEditor(
  APicture: TPicture; AGraphicClass: TGraphicClass; const ABuildFilter: string; AImportList: TStrings): Boolean;

  procedure InitInternalControls;
  begin
    OpenDialog.Options := [ofHideReadOnly, ofFileMustExist, ofShowHelp];
    SaveDialog.Options := [ofHideReadOnly, ofFileMustExist, ofShowHelp];
    OpenDialog.DefaultExt := cxGraphicExtension(AGraphicClass);
    if ABuildFilter <> '' then
      OpenDialog.Filter := ABuildFilter
    else
      OpenDialog.Filter := cxGraphicFilter(AGraphicClass);

    btnImport.Visible := AImportList <> nil;
    btnImport.Enabled := (AImportList <> nil) and (AImportList.Count > 0);

    tsPictureEditor.TabVisible := True;
    pcMain.ActivePage := tsPictureEditor;
    gcIcons.OptionsBehavior.ItemCheckMode := icmSingleRadio;
  end;

var
  ACheckedItem: TdxGalleryControlItem;
  APNGImage: TdxPNGImage;
begin
  FPicture.Assign(APicture);
  ImportList := AImportList;
  gcIcons.PopupMenu := nil;
  InitInternalControls;
  PopulateContent;
  PopulateGallery;
  Result := ShowModal = mrOk;
  if Result then
  begin
   if (pcMain.ActivePage = tsDXImageGallery) then
    begin
      ACheckedItem := gcIcons.Gallery.GetCheckedItem;
      if (ACheckedItem <> nil) and ACheckedItem.Group.Visible then
      begin
        APNGImage := TdxPNGImage.Create;
        try
          APNGImage.Assign(ACheckedItem.Glyph);
          APicture.Assign(APNGImage);
        finally
          APNGImage.Free;
        end;
      end;
    end
    else
      APicture.Assign(FPicture);
  end;
end;

function TdxfmImagePicker.GetGalleryItem(AIconLibraryImage: TdxIconLibraryImage): TdxGalleryControlItem;
begin
  Result := TdxGalleryControlItem(AIconLibraryImage.Tag);
end;

function TdxfmImagePicker.GetIconLibraryImage(AGalleryControlItem: TdxGalleryControlItem): TdxIconLibraryImage;
begin
  Result := TdxIconLibraryImage(AGalleryControlItem.Tag);
end;

procedure TdxfmImagePicker.DrawPicture(ACanvas: TCanvas; R: TRect);
begin
  cxDrawTransparencyCheckerboard(ACanvas.Handle, R);
  ACanvas.Brush.Color := Color;
  if (FPicture.Width > 0) and (FPicture.Height > 0) then
  begin
    cxPaintCanvas.BeginPaint(ACanvas);
    try
      cxDrawPicture(cxPaintCanvas, R, FPicture, ifmFit);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end
  else
  begin
    R := cxRectCenter(R, Size(FPicture.Width, FPicture.Height));
    ACanvas.Brush.Style := bsClear;
    ACanvas.TextOut(R.Left, R.Top, 'None');
  end;
end;

procedure TdxfmImagePicker.PopulateContent;
var
  APopulateContentHelper: TdxPopulateContentHelper;
begin
  APopulateContentHelper := TdxPopulateContentHelper.Create(Self);
  try
    APopulateContentHelper.Populate;
  finally
    APopulateContentHelper.Free;
  end;
end;

procedure TdxfmImagePicker.PopulateGallery;
begin
  if FPopulateGalleryLockCount = 0 then
    FPopulateGalleryHelper.Populate;
end;

procedure TdxfmImagePicker.SetImportList(AValue: TStrings);
var
  I: Integer;
  AMenuItem: TMenuItem;
begin
  if AValue <> nil then
    FImportList.Assign(AValue)
  else
    FImportList.Clear;

  pmImport.Items.Clear;
  for I := 0 to FImportList.Count - 1 do
  begin
    AMenuItem := TMenuItem.Create(Self);
    AMenuItem.Caption := FImportList[I];
    AMenuItem.Tag := I;
    AMenuItem.OnClick := ImportPictureClick;
    pmImport.Items.Add(AMenuItem);
  end;
end;

procedure TdxfmImagePicker.UpdateGalleryItemsSelection(ASelect: Boolean);

  procedure UpdateGalleryItemsSelectionInGroup(AGroup: TdxGalleryControlGroup);
  var
    I: Integer;
  begin
    for I := 0 to AGroup.ItemCount - 1 do
      AGroup.Items[I].Checked := ASelect;
  end;

var
  I: Integer;
begin
  gcIcons.BeginUpdate;
  try
    for I := 0 to gcIcons.Gallery.Groups.Count - 1 do
      UpdateGalleryItemsSelectionInGroup(gcIcons.Gallery.Groups[I]);
  finally
    gcIcons.EndUpdate;
  end;
end;

procedure TdxfmImagePicker.ImportPictureClick(Sender: TObject);
begin
  FPicture.Assign(TPersistent(FImportList.Objects[TMenuItem(Sender).Tag]));
end;

procedure TdxfmImagePicker.PictureChangeHandler(Sender: TObject);
begin
  ImagePaintBox.Invalidate;
end;

procedure TdxfmImagePicker.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChangeHandler;
  FImportList := TStringList.Create;

  FPopulateGalleryMode := gpmGroups;
  FPopulateGalleryHelper := TdxPopulateGalleryHelper.Create(Self);

  clbSize.InnerCheckListBox.MultiSelect := True;
  clbCollection.InnerCheckListBox.MultiSelect := True;
  clbCategories.InnerCheckListBox.MultiSelect := True;
end;

procedure TdxfmImagePicker.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPopulateGalleryHelper);
  FreeAndNil(FImportList);
  FreeAndNil(FPicture);
end;

procedure TdxfmImagePicker.beFindPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  beFind.Text := '';
end;

procedure TdxfmImagePicker.beFindPropertiesChange(Sender: TObject);
const
  PopulateModeToImageIndex: array[TdxPopulateGalleryMode] of Integer = (0, 1);
begin
  if beFind.Text <> '' then
    FPopulateGalleryMode := gpmSearch
  else
    FPopulateGalleryMode := gpmGroups;

  beFind.Properties.Buttons[0].ImageIndex := PopulateModeToImageIndex[FPopulateGalleryMode];
  PopulateGallery;
end;

procedure TdxfmImagePicker.clbCategoriesClickCheck(
  Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
begin
  gcIcons.Gallery.Groups[AIndex].Visible := (ANewState = cbsChecked) and (FPopulateGalleryMode = gpmGroups);
  PopulateGallery;
end;

procedure TdxfmImagePicker.clbCollectionClickCheck(
  Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
begin
  PopulateGallery;
end;

procedure TdxfmImagePicker.gcIconsDblClick(Sender: TObject);
var
  AItem: TdxGalleryControlItem;
begin
  AItem := gcIcons.Gallery.Groups.GetItemAtPos(gcIcons.MouseDownPos);
  if AItem <> nil then
  begin
    UpdateGalleryItemsSelection(False);
    AItem.Checked := True;
    FPicture.Graphic := GetIconLibraryImage(AItem).Image;
    ModalResult := mrOk;
  end;
end;

procedure TdxfmImagePicker.ImagePaintBoxMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // to make shortcuts work
  pnlImagePaintBox.SetFocus;
end;

procedure TdxfmImagePicker.ImagePaintBoxPaint(Sender: TObject);
begin
  DrawPicture(ImagePaintBox.Canvas, ImagePaintBox.ClientRect);
end;

procedure TdxfmImagePicker.miImageCopyClick(Sender: TObject);
begin
  if IsPictureAssigned(FPicture) then
  begin
    Clipboard.Assign(FPicture);
    if TComponent(Sender).Tag = 1 then
      btnClear.Click;
  end;
end;

procedure TdxfmImagePicker.miImagePasteClick(Sender: TObject);
var
  AImage: TdxSmartImage;
begin
  if TdxSmartImage.HasClipboardFormat then
  begin
    AImage := TdxSmartImage.Create;
    try
      AImage.PasteFromClipboard;
      FPicture.Graphic := AImage;
    finally
      AImage.Free;
    end;
  end;
end;

procedure TdxfmImagePicker.miSelectAllinThisGroupClick(Sender: TObject);
var
  I: Integer;
begin
  if FSelectedGroup <> nil then
  begin
    gcIcons.BeginUpdate;
    try
      for I := 0 to FSelectedGroup.ItemCount - 1 do
        FSelectedGroup.Items[I].Checked := TComponent(Sender).Tag <> 0;
    finally
      gcIcons.EndUpdate;
    end;
  end;
end;

procedure TdxfmImagePicker.miSelectClick(Sender: TObject);
var
  ACheckListBox: TcxCustomCheckListBox;
  I: Integer;
  AActiveComponent: TComponent;
begin
  AActiveComponent := TComponent(ActiveControl);
  if AActiveComponent = gcIcons then
    UpdateGalleryItemsSelection(TComponent(Sender).Tag <> 0)
  else
    if AActiveComponent is TcxCustomInnerCheckListBox then
    begin
      ACheckListBox := TcxCustomInnerCheckListBox(AActiveComponent).Container;
      for I := 0 to ACheckListBox.Items.Count - 1 do
        ACheckListBox.Selected[I] := TComponent(Sender).Tag <> 0;
    end;
end;

procedure TdxfmImagePicker.miUncheckSelectedClick(Sender: TObject);
const
  StateMap: array[Boolean] of TcxCheckBoxState = (cbsUnchecked, cbsChecked);
var
  ACheckListBox: TcxCheckListBox;
  ANewState: TcxCheckBoxState;
  APrevState: TcxCheckBoxState;
  I: Integer;
begin
  if pmSelection.PopupComponent is TcxCustomInnerCheckListBox then
  begin
    ACheckListBox := TcxCustomInnerCheckListBox(pmSelection.PopupComponent).Container as TcxCheckListBox;
    gcIcons.BeginUpdate;
    try
      Inc(FPopulateGalleryLockCount);
      try
        for I := 0 to ACheckListBox.Items.Count - 1 do
        begin
          if ACheckListBox.Selected[I] then
          begin
            ANewState := StateMap[TComponent(Sender).Tag <> 0];
            APrevState := StateMap[ACheckListBox.Items[I].Checked];
            if APrevState <> ANewState then
            begin
              ACheckListBox.Items[I].Checked := ANewState = cbsChecked;
              if Assigned(ACheckListBox.OnClickCheck) then
                ACheckListBox.OnClickCheck(ACheckListBox, I, APrevState, ANewState);
            end;
          end;
        end;
      finally
        Dec(FPopulateGalleryLockCount);
        PopulateGallery;
      end;
    finally
      gcIcons.EndUpdate;
    end;
  end;
end;

procedure TdxfmImagePicker.pmPictureEditorPopup(Sender: TObject);
begin
  miImageCut.Enabled := IsPictureAssigned(FPicture);
  miImageCopy.Enabled := miImageCut.Enabled;
  miImagePaste.Enabled := TdxSmartImage.HasClipboardFormat;
end;

procedure TdxfmImagePicker.pmSelectionPopup(Sender: TObject);
begin
  FSelectedGroup := gcIcons.Gallery.Groups.GetGroupAtPos(gcIcons.ScreenToClient(GetMouseCursorPos));
  miCheckSelected.Visible := pmSelection.PopupComponent is TcxCustomInnerCheckListBox;
  miUncheckSelected.Visible := pmSelection.PopupComponent is TcxCustomInnerCheckListBox;
  miSelectAllinThisGroup.Visible := FSelectedGroup <> nil;
  miUnselectAllinThisGroup.Visible := FSelectedGroup <> nil;
end;

procedure TdxfmImagePicker.actClearExecute(Sender: TObject);
begin
  FPicture.Graphic := nil;
end;

procedure TdxfmImagePicker.actClearUpdate(Sender: TObject);
begin
  actClear.Enabled := IsPictureAssigned(FPicture);
end;

procedure TdxfmImagePicker.actF3Execute(Sender: TObject);
begin
  beFind.SetFocus;
end;

procedure TdxfmImagePicker.actLoadExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    FPicture.LoadFromFile(OpenDialog.Filename);
end;

procedure TdxfmImagePicker.actSaveExecute(Sender: TObject);
begin
  SaveDialog.Filter := cxGraphicFilter(FPicture.Graphic, True);
  SaveDialog.DefaultExt := cxGraphicExtension(FPicture.Graphic);
  if SaveDialog.Execute then
    FPicture.SaveToFile(SaveDialog.Filename);
end;

procedure TdxfmImagePicker.actSaveUpdate(Sender: TObject);
begin
  actSave.Enabled := IsPictureAssigned(FPicture);
end;

procedure TdxfmImagePicker.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  if beFind.Focused then
    case Msg.CharCode of
      VK_ESCAPE:
        begin
          beFind.Text := '';
          Handled := True;
        end;
      VK_RETURN:
        begin
          gcIcons.SetFocus;
          Handled := True;
        end;
    end;
end;

procedure TdxfmImagePicker.btnImportClick(Sender: TObject);
begin
//
end;

procedure TdxfmImagePicker.pcMainChange(Sender: TObject);
begin
  if pcMain.ActivePage = tsDXImageGallery then
    ActiveControl := gcIcons;
end;

initialization
  CreateIconLibrary;

finalization
  DestroyIconLibrary;

end.
