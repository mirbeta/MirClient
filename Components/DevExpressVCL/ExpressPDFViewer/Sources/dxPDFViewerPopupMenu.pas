{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
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

unit dxPDFViewerPopupMenu;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, ImgList, Menus, dxCore, cxControls, cxGraphics, dxBuiltInPopupMenu,
  dxPDFCore, dxPDFViewer;

type
  { TdxPDFViewerCustomPopupMenu }

  TdxPDFViewerCustomPopupMenu = class(TComponent)
  strict private
    FAdapter: TdxCustomBuiltInPopupMenuAdapter;
    FImageList: TcxImageList;

    function GetInternalPopupMenu: TComponent;
    procedure MenuItemClick(Sender: TObject);
  protected
    function AddImage(const AResourceName: string): Integer;
    function AddMenuItem(ACaption: Pointer; ACommandID: Word; const AImageResName: string = '';
      AEnabled: Boolean = True; AShortCut: TShortCut = 0; AParent: TComponent = nil): TComponent; overload;
    function AddMenuItem(ACaption: Pointer; ACommandID, AParam: Word; const AImageResName: string = '';
      AEnabled: Boolean = True; AShortCut: TShortCut = 0; AParent: TComponent = nil): TComponent; overload;
    function GetViewer: TdxPDFCustomViewer; virtual;
    procedure AddSeparator;
    procedure PopulateMenuItems; virtual;
    procedure ProcessClick(ACommand, AParam: Word); virtual; abstract;

    property InternalPopupMenu: TComponent read GetInternalPopupMenu;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize;
    function Popup(const P: TPoint): Boolean;

    property Adapter: TdxCustomBuiltInPopupMenuAdapter read FAdapter;
    property ImageList: TcxImageList read FImageList;
    property Viewer: TdxPDFCustomViewer read GetViewer;
  end;

  { TdxPDFViewerImagePopupMenu }

  TdxPDFViewerImagePopupMenu = class(TdxPDFViewerCustomPopupMenu)
  strict private const
    ccCopy = 1;
  protected
    procedure PopulateMenuItems; override;
    procedure ProcessClick(ACommand, AParam: Word); override;

    function GetCopyMenuCaption: Pointer; virtual;
  end;

  { TdxPDFViewerTextPopupMenu }

  TdxPDFViewerTextPopupMenu = class(TdxPDFViewerImagePopupMenu)
  protected
    function GetCopyMenuCaption: Pointer; override;
  end;

  { TdxPDFViewerAttachmentPopupMenu }

  TdxPDFViewerAttachmentPopupMenu = class(TdxPDFViewerCustomPopupMenu)
  strict private const
    OpenFileID = 0;
    SaveFileID = 1;
  protected
    procedure PopulateMenuItems; override;
    procedure ProcessClick(ACommand, AParam: Word); override;

    function GetAttachment: TdxPDFFileAttachment; virtual;
    function GetOpenFileCaption: Pointer; virtual;
    function GetSaveFileCaption: Pointer; virtual;
  end;

  { TdxPDFViewerNavigationPaneAttachmentPopupMenu }

  TdxPDFViewerNavigationPaneAttachmentPopupMenu = class(TdxPDFViewerAttachmentPopupMenu)
  protected
    function GetAttachment: TdxPDFFileAttachment; override;
  end;

 { TdxPDFViewerPagePopupMenu }

  TdxPDFViewerPagePopupMenu = class(TdxPDFViewerCustomPopupMenu)
  strict private const
    HandToolCommandID = 12;
    SelectToolCommandID = 13;
    SelectAllTextCommandID = 14;

    ActualSizeCommandID = 3;
    FitWidthCommandID = 5;
    ZoomInCommandID = 1;
    ZoomOutCommandID = 2;
    ZoomToPageLevelCommandID = 4;

    NextViewCommandID = 11;
    PreviousViewCommandID = 10;
    RotateClockwiseCommandID = 8;
    RotateCounterclockwiseCommandID = 9;

    PrintCommandID = 6;
    FindCommandID = 15;

    ShowDocumentPropertiesCommandID = 7;

    procedure PopulateDocumentPropertiesMenuItems;
    procedure PopulatePrintMenuItems;
    procedure PopulateRotateMenuItems;
    procedure PopulateSelectAllItem;
    procedure PopulateToolsMenuItems;
    procedure PopulateViewStateHistoryMenuItems;
    procedure PopulateZoomMenuItems;
    procedure PopulateZoomModeMenuItems;
  protected
    procedure PopulateMenuItems; override;
    procedure ProcessClick(ACommand, AParam: Word); override;
  end;

  { TdxPDFViewerFindPanelOptionsPopupMenu }

  TdxPDFViewerFindPanelOptionsPopupMenu = class(TdxPDFViewerCustomPopupMenu)
  strict private const
    CaseSensitiveCommandID = 0;
    WholeWordsCommandID = 1;
  protected
    procedure PopulateMenuItems; override;
    procedure ProcessClick(ACommand, AParam: Word); override;

    function GetCaseSensitiveMenuCaption: Pointer; virtual;
    function GetWholeWordsMenuCaption: Pointer; virtual;
  end;

  { TdxPDFViewerBookmarkPopupMenu }

  TdxPDFViewerBookmarkPopupMenu = class(TdxPDFViewerCustomPopupMenu)
  strict private const
    GoToBookmarkCommandID = 1;
    PrintPagesCommandID = 2;
    PrintSectionsCommandID = 3;
  strict private
    function GetBookmarks: TdxPDFViewerBookmarks;
  protected
    procedure PopulateMenuItems; override;
    procedure ProcessClick(ACommand, AParam: Word); override;

    function GetGoToBookmarkCaption: Pointer; virtual;
    function GetPrintPagesCaption: Pointer; virtual;
    function GetPrintSectionsCaption: Pointer; virtual;

    property Bookmarks: TdxPDFViewerBookmarks read GetBookmarks;
  end;

  { TdxPDFViewerBookmarksPageOptionsPopupMenu }

  TdxPDFViewerBookmarksPageOptionsPopupMenu = class(TdxPDFViewerBookmarkPopupMenu)
  strict private const
    CollapseTopLevelBookmarksCommandID = 10;
    ExpandCurrentBookmarkCommandID = 11;
    HideAfterUseCommandID = 12;
    SetLargeTextSizeCommandID = 13;
    SetMediumTextSizeCommandID = 14;
    SetSmallTextSizeCommandID = 15;
  strict private
    function GetBookmarks: TdxPDFViewerBookmarks;
  protected
    procedure PopulateMenuItems; override;
    procedure ProcessClick(ACommand, AParam: Word); override;

    function GetCollapseTopLevelBookmarksCaption: Pointer; virtual;
    function GetExpandCurrentBookmarkCaption: Pointer; virtual;
    function GetHideAfterUseCaption: Pointer; virtual;
    function GetLargeTextSizeCaption: Pointer; virtual;
    function GetMediumTextSizeCaption: Pointer; virtual;
    function GetSmallTextSizeCaption: Pointer; virtual;
    function GetTextSizeCaption: Pointer; virtual;

    property Bookmarks: TdxPDFViewerBookmarks read GetBookmarks;
  end;

  { TdxPDFViewerThumbnailsPopupMenu }

  TdxPDFViewerThumbnailsPopupMenu = class(TdxPDFViewerCustomPopupMenu)
  strict private const
    EnlargePageThumbnailsID = 0;
    PrintPagesCommandID = 1;
    ReducePageThumbnailsID = 2;
  strict private
    function GetThumbnails: TdxPDFViewerThumbnails;
  protected
    function GetViewer: TdxPDFCustomViewer; override;
    procedure PopulateMenuItems; override;
    procedure ProcessClick(ACommand, AParam: Word); override;

    function GetEnlargePageThumbnailsCaption: Pointer; virtual;
    function GetPrintPagesCaption: Pointer; virtual;
    function GetReducePageThumbnailsCaption: Pointer; virtual;

    property Thumbnails: TdxPDFViewerThumbnails read GetThumbnails;
  end;

implementation

uses
  SysUtils, dxGDIPlusClasses, dxCustomPreview, dxPDFDocument, dxPDFViewerDocumentPropertiesDialog, dxPDFViewerActionsStrs,
  dxPDFViewerDialogsStrs;

{$R dxPDFViewerPopupMenu.res}

type
  TdxPDFViewerAccess = class(TdxPDFCustomViewer);
  TdxPDFViewerAttachmentsAccess = class(TdxPDFViewerAttachments);
  TdxPDFViewerBookmarksAccess = class(TdxPDFViewerBookmarks);
  TdxPDFViewerControllerAccess = class(TdxPDFViewerController);
  TdxPDFViewerOptionsFindPanelAccess = class(TdxPDFViewerOptionsFindPanel);
  TdxPDFViewerThumbnailsAccess = class(TdxPDFViewerThumbnails);

{ TdxPDFViewerCustomPopupMenu }

constructor TdxPDFViewerCustomPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageList := TcxImageList.Create(Self);
  FAdapter := TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass.Create(Self);
end;

destructor TdxPDFViewerCustomPopupMenu.Destroy;
begin
  FreeAndNil(FAdapter);
  FreeAndNil(FImageList);
  inherited Destroy;
end;

procedure TdxPDFViewerCustomPopupMenu.Initialize;
begin
  Adapter.Clear;
  Adapter.SetImages(nil);
  ImageList.Clear;
  Adapter.SetLookAndFeel(TdxPDFViewerAccess(Viewer).LookAndFeel);
  Adapter.SetImages(ImageList);
  PopulateMenuItems;
end;

function TdxPDFViewerCustomPopupMenu.Popup(const P: TPoint): Boolean;
begin
  Initialize;
  Result := Adapter.Popup(P);
end;

function TdxPDFViewerCustomPopupMenu.AddImage(const AResourceName: string): Integer;
var
  APNGImage: TdxPNGImage;
begin
  Result := -1;
  if AResourceName <> '' then
  begin
    APNGImage := TdxPNGImage.Create;
    try
      APNGImage.LoadFromResource(HInstance, AResourceName, 'PNG');
      Result := ImageList.Add(APNGImage);
    finally
      APNGImage.Free;
    end;
  end;
end;

function TdxPDFViewerCustomPopupMenu.AddMenuItem(ACaption: Pointer; ACommandID: Word;
  const AImageResName: string = ''; AEnabled: Boolean = True; AShortCut: TShortCut = 0; AParent: TComponent = nil): TComponent;
begin
  Result := AddMenuItem(ACaption, ACommandID, 0, AImageResName, AEnabled, AShortCut, AParent);
end;

function TdxPDFViewerCustomPopupMenu.AddMenuItem(ACaption: Pointer; ACommandID, AParam: Word;
  const AImageResName: string = ''; AEnabled: Boolean = True; AShortCut: TShortCut = 0; AParent: TComponent = nil): TComponent;
begin
  Result := Adapter.Add(cxGetResourceString(ACaption), MenuItemClick,
    MakeLong(ACommandID, AParam), AddImage(AImageResName), AEnabled, AShortCut, AParent);
end;

procedure TdxPDFViewerCustomPopupMenu.PopulateMenuItems;
begin

end;

function TdxPDFViewerCustomPopupMenu.GetViewer: TdxPDFCustomViewer;
begin
  Result := Owner as TdxPDFCustomViewer;
end;

procedure TdxPDFViewerCustomPopupMenu.AddSeparator;
begin
  Adapter.AddSeparator;
end;

function TdxPDFViewerCustomPopupMenu.GetInternalPopupMenu: TComponent;
begin
  Result := Adapter.PopupMenu;
end;

procedure TdxPDFViewerCustomPopupMenu.MenuItemClick(Sender: TObject);
begin
  ProcessClick(Word(TComponent(Sender).Tag), HiWord(TComponent(Sender).Tag));
end;

{ TdxPDFViewerImagePopupMenu }

procedure TdxPDFViewerImagePopupMenu.PopulateMenuItems;
begin
  AddMenuItem(GetCopyMenuCaption, ccCopy, 'DXPDFVIEWER_POPUPMENU_GLYPH_COPY', TdxPDFViewerAccess(Viewer).CanExtractContent);
end;

procedure TdxPDFViewerImagePopupMenu.ProcessClick(ACommand, AParam: Word);
begin
  Viewer.Selection.CopyToClipboard;
end;

function TdxPDFViewerImagePopupMenu.GetCopyMenuCaption: Pointer;
begin
  Result := @sdxPDFViewerPopupMenuCopyImage;
end;

{ TdxPDFViewerTextPopupMenu }

function TdxPDFViewerTextPopupMenu.GetCopyMenuCaption: Pointer;
begin
  Result := @sdxPDFViewerPopupMenuCopyText;
end;

{ TdxPDFViewerAttachmentPopupMenu }

procedure TdxPDFViewerAttachmentPopupMenu.PopulateMenuItems;
begin
  AddMenuItem(GetOpenFileCaption, OpenFileID, 'DXPDFVIEWER_POPUPMENU_GLYPH_OPENFILE');
  AddMenuItem(GetSaveFileCaption, SaveFileID, 'DXPDFVIEWER_POPUPMENU_GLYPH_SAVEFILE');
end;

procedure TdxPDFViewerAttachmentPopupMenu.ProcessClick(ACommand, AParam: Word);
begin
  case ACommand of
    OpenFileID:
      TdxPDFViewerAccess(Viewer).OpenAttachment(GetAttachment);
    SaveFileID:
      TdxPDFViewerAccess(Viewer).SaveAttachment(GetAttachment);
  end;
end;

function TdxPDFViewerAttachmentPopupMenu.GetAttachment: TdxPDFFileAttachment;
begin
  Result := TdxPDFViewerAccess(Viewer).FocusedCellAsAttachment;
end;

function TdxPDFViewerAttachmentPopupMenu.GetOpenFileCaption: Pointer;
begin
  Result := @sdxPDFViewerPopupMenuOpenAttachmentFileText;
end;

function TdxPDFViewerAttachmentPopupMenu.GetSaveFileCaption: Pointer;
begin
  Result := @sdxPDFViewerPopupMenuSaveAttachmentFileText;
end;

{ TdxPDFViewerNavigationPaneAttachmentPopupMenu }

function TdxPDFViewerNavigationPaneAttachmentPopupMenu.GetAttachment: TdxPDFFileAttachment;
begin
  Result := TdxPDFViewerAttachmentsAccess(TdxPDFViewerAccess(Viewer).Attachments).SelectedAttachment;
end;

{ TdxPDFViewerPagePopupMenu }

procedure TdxPDFViewerPagePopupMenu.PopulateMenuItems;
begin
  PopulateToolsMenuItems;
  PopulateZoomMenuItems;
  PopulateZoomModeMenuItems;
  PopulateViewStateHistoryMenuItems;
  PopulateRotateMenuItems;
  PopulatePrintMenuItems;
  PopulateSelectAllItem;
  PopulateDocumentPropertiesMenuItems;
end;

procedure TdxPDFViewerPagePopupMenu.ProcessClick(ACommand, AParam: Word);
begin
  case ACommand of
    SelectToolCommandID:
      Viewer.HandTool := False;
    HandToolCommandID:
      Viewer.HandTool := True;
    ZoomInCommandID:
      Viewer.ZoomIn;
    ZoomOutCommandID:
      Viewer.ZoomOut;
    ActualSizeCommandID:
      TdxPDFViewerAccess(Viewer).OptionsZoom.ZoomFactor := 100;
    ZoomToPageLevelCommandID:
      TdxPDFViewerAccess(Viewer).OptionsZoom.ZoomMode := pzmPages;
    FitWidthCommandID:
      TdxPDFViewerAccess(Viewer).OptionsZoom.ZoomMode := pzmPageWidth;
    PreviousViewCommandID:
      Viewer.GoToPrevView;
    NextViewCommandID:
      Viewer.GoToNextView;
    RotateClockwiseCommandID:
      Viewer.RotateClockwise;
    RotateCounterclockwiseCommandID:
      Viewer.RotateCounterclockwise;
    PrintCommandID:
      ShowPrintDialog(Viewer);
    FindCommandID:
      if not Viewer.IsFindPanelVisible then
        Viewer.ShowFindPanel
      else
        Viewer.HideFindPanel;
    SelectAllTextCommandID:
      Viewer.Selection.SelectAll;
    ShowDocumentPropertiesCommandID:
      ShowDocumentPropertiesDialog(Viewer);
  end;
end;

procedure TdxPDFViewerPagePopupMenu.PopulateDocumentPropertiesMenuItems;
begin
  AddSeparator;
  AddMenuItem(@sdxPDFViewerPopupMenuDocumentProperties, ShowDocumentPropertiesCommandID);
end;

procedure TdxPDFViewerPagePopupMenu.PopulatePrintMenuItems;

  function NeedShowFindPanelMenuItem: Boolean;
  begin
    Result := TdxPDFViewerAccess(Viewer).CanChangeVisibility and TdxPDFViewerAccess(Viewer).CanShowFindPanel;
  end;

var
  AItem: TComponent;
begin
  if TdxPDFViewerAccess(Viewer).CanPrint then
  begin
    AddSeparator;
    AddMenuItem(@sdxPDFViewerPopupMenuPrint, PrintCommandID, 'DXPDFVIEWER_POPUPMENU_GLYPH_PRINT',
      TdxPDFViewerAccess(Viewer).CanPrint);
  end
  else
    if not NeedShowFindPanelMenuItem then
      AddSeparator;
  if NeedShowFindPanelMenuItem then
  begin
    AItem := AddMenuItem(@sdxPDFViewerActionFindCaption, FindCommandID, 'DXPDFVIEWER_POPUPMENU_GLYPH_FIND');
    Adapter.SetChecked(AItem, Viewer.IsFindPanelVisible);
  end;
end;

procedure TdxPDFViewerPagePopupMenu.PopulateRotateMenuItems;
begin
  AddSeparator;
  AddMenuItem(@sdxPDFViewerActionRotateClockwiseCaption, RotateClockwiseCommandID,
    'DXPDFVIEWER_POPUPMENU_GLYPH_ROTATECLOCKWISE');
  AddMenuItem(@sdxPDFViewerActionRotateCounterclockwiseCaption, RotateCounterclockwiseCommandID,
    'DXPDFVIEWER_POPUPMENU_GLYPH_ROTATECOUNTERCLOCKWISE');
end;

procedure TdxPDFViewerPagePopupMenu.PopulateSelectAllItem;
begin
  if TdxPDFViewerAccess(Viewer).IsDocumentLoaded then
  begin
    AddSeparator;
    AddMenuItem(@sdxPDFViewerActionSelectAllCaption, SelectAllTextCommandID, 'DXPDFVIEWER_POPUPMENU_GLYPH_SELECTALL');
  end;
end;

procedure TdxPDFViewerPagePopupMenu.PopulateToolsMenuItems;
var
  AItem: TComponent;
begin
  AItem := AddMenuItem(@sdxPDFViewerActionSelectToolCaption, SelectToolCommandID,
    'DXPDFVIEWER_POPUPMENU_GLYPH_SELECTTOOL');
  Adapter.SetChecked(AItem, not Viewer.HandTool);

  AItem := AddMenuItem(@sdxPDFViewerActionHandToolCaption, HandToolCommandID,
    'DXPDFVIEWER_POPUPMENU_GLYPH_HANDTOOL');
  Adapter.SetChecked(AItem, Viewer.HandTool);
end;

procedure TdxPDFViewerPagePopupMenu.PopulateViewStateHistoryMenuItems;
begin
  if Viewer.CanGoToPrevView or Viewer.CanGoToNextView then
  begin
    AddSeparator;
    if Viewer.CanGoToPrevView then
      AddMenuItem(@sdxPDFViewerActionGoToPrevViewCaption, PreviousViewCommandID, 'DXPDFVIEWER_POPUPMENU_GLYPH_PREVIOUSVIEW');
    if Viewer.CanGoToNextView then
      AddMenuItem(@sdxPDFViewerActionGoToNextViewCaption, NextViewCommandID, 'DXPDFVIEWER_POPUPMENU_GLYPH_NEXTVIEW');
  end;
end;

procedure TdxPDFViewerPagePopupMenu.PopulateZoomMenuItems;
begin
  AddSeparator;
  AddMenuItem(@sdxPDFViewerActionZoomInCaption, ZoomInCommandID, 'DXPDFVIEWER_POPUPMENU_GLYPH_ZOOMIN',
    TdxPDFViewerAccess(Viewer).CanZoomIn);
  AddMenuItem(@sdxPDFViewerActionZoomOutCaption, ZoomOutCommandID, 'DXPDFVIEWER_POPUPMENU_GLYPH_ZOOMOUT',
    TdxPDFViewerAccess(Viewer).CanZoomOut);
end;

procedure TdxPDFViewerPagePopupMenu.PopulateZoomModeMenuItems;
var
  AItem: TComponent;
begin
  AddSeparator;
  AItem := AddMenuItem(@sdxPDFViewerActionZoomActualSizeCaption, ActualSizeCommandID,
    'DXPDFVIEWER_POPUPMENU_GLYPH_ACTUALSIZE');
  Adapter.SetChecked(AItem, TdxPDFViewerAccess(Viewer).ZoomFactor = 100);
  AItem := AddMenuItem(@sdxPDFViewerActionZoomToPageLevelCaption, ZoomToPageLevelCommandID);
  Adapter.SetChecked(AItem, TdxPDFViewerAccess(Viewer).ZoomMode = pzmPages);
  AItem := AddMenuItem(@sdxPDFViewerActionZoomFitWidthCaption, FitWidthCommandID);
  Adapter.SetChecked(AItem, TdxPDFViewerAccess(Viewer).ZoomMode = pzmPageWidth);
end;

{ TdxPDFViewerFindPanelOptionsPopupMenu }

procedure TdxPDFViewerFindPanelOptionsPopupMenu.PopulateMenuItems;
var
  AItem: TComponent;
begin
  AItem := AddMenuItem(GetCaseSensitiveMenuCaption, CaseSensitiveCommandID);
  Adapter.SetChecked(AItem, TdxPDFViewerAccess(Viewer).OptionsFindPanel.CaseSensitive);
  AItem := AddMenuItem(GetWholeWordsMenuCaption, WholeWordsCommandID);
  Adapter.SetChecked(AItem, TdxPDFViewerAccess(Viewer).OptionsFindPanel.WholeWords);
end;

procedure TdxPDFViewerFindPanelOptionsPopupMenu.ProcessClick(ACommand, AParam: Word);
var
  AViewer: TdxPDFViewerAccess;
begin
  AViewer := TdxPDFViewerAccess(Viewer);
  case ACommand of
    CaseSensitiveCommandID:
      AViewer.OptionsFindPanel.CaseSensitive := not AViewer.OptionsFindPanel.CaseSensitive;
    WholeWordsCommandID:
      AViewer.OptionsFindPanel.WholeWords := not AViewer.OptionsFindPanel.WholeWords;
  end;
end;

function TdxPDFViewerFindPanelOptionsPopupMenu.GetCaseSensitiveMenuCaption: Pointer;
begin
  Result := @sdxPDFViewerFindPanelPopupMenuCaseSensitive;
end;

function TdxPDFViewerFindPanelOptionsPopupMenu.GetWholeWordsMenuCaption: Pointer;
begin
  Result := @sdxPDFViewerFindPanelPopupMenuWholeWords;
end;

{ TdxPDFViewerBookmarkPopupMenu }

procedure TdxPDFViewerBookmarkPopupMenu.PopulateMenuItems;
var
  ABookmarks: TdxPDFViewerBookmarksAccess;
begin
  ABookmarks := TdxPDFViewerBookmarksAccess(Bookmarks);
  AddMenuItem(GetGoToBookmarkCaption, GoToBookmarkCommandID, '', ABookmarks.IsBookmarkSelected);
  if TdxPDFViewerAccess(Viewer).CanPrint and ABookmarks.IsBookmarkSelected then
  begin
    AddMenuItem(GetPrintPagesCaption, PrintPagesCommandID);
    AddMenuItem(GetPrintSectionsCaption, PrintSectionsCommandID);
  end;
end;

procedure TdxPDFViewerBookmarkPopupMenu.ProcessClick(ACommand, AParam: Word);
begin
  case ACommand of
    GoToBookmarkCommandID:
      TdxPDFViewerAccess(Viewer).Bookmarks.GoToBookmark;
    PrintPagesCommandID:
      TdxPDFViewerAccess(Viewer).Bookmarks.PrintPages;
    PrintSectionsCommandID:
      TdxPDFViewerAccess(Viewer).Bookmarks.PrintSections;
  end;
end;

function TdxPDFViewerBookmarkPopupMenu.GetGoToBookmarkCaption: Pointer;
begin
  Result := @sdxPDFViewerBookmarkPopupMenuGoToBookmark;
end;

function TdxPDFViewerBookmarkPopupMenu.GetPrintPagesCaption: Pointer;
begin
  Result := @sdxPDFViewerBookmarkPopupMenuPrintPages;
end;

function TdxPDFViewerBookmarkPopupMenu.GetPrintSectionsCaption: Pointer;
begin
  Result := @sdxPDFViewerBookmarkPopupMenuPrintSections;
end;

function TdxPDFViewerBookmarkPopupMenu.GetBookmarks: TdxPDFViewerBookmarks;
begin
  Result := TdxPDFViewerAccess(Viewer).Bookmarks;
end;

{ TdxPDFViewerBookmarksPageOptionsPopupMenu }

procedure TdxPDFViewerBookmarksPageOptionsPopupMenu.PopulateMenuItems;
var
  AItem, AParent: TComponent;
  ABookmarks: TdxPDFViewerBookmarksAccess;
begin
  ABookmarks := TdxPDFViewerBookmarksAccess(Bookmarks);
  AddMenuItem(GetExpandCurrentBookmarkCaption, ExpandCurrentBookmarkCommandID, '', ABookmarks.CanExpandCurrentBookmark);

  AddSeparator;
  AddMenuItem(GetCollapseTopLevelBookmarksCaption, CollapseTopLevelBookmarksCommandID, '', not ABookmarks.Empty);

  AddSeparator;
  AItem := AddMenuItem(GetHideAfterUseCaption, HideAfterUseCommandID);
  Adapter.SetChecked(AItem, TdxPDFViewerAccess(Viewer).OptionsNavigationPane.Bookmarks.HideAfterUse);

  AddSeparator;
  AParent := Adapter.AddSubMenu(cxGetResourceString(GetTextSizeCaption), nil);
  AItem := AddMenuItem(GetSmallTextSizeCaption, SetSmallTextSizeCommandID, '', True, 0, AParent);
  Adapter.SetChecked(AItem, TdxPDFViewerAccess(Viewer).OptionsNavigationPane.Bookmarks.TextSize = btsSmall);
  AItem := AddMenuItem(GetMediumTextSizeCaption, SetMediumTextSizeCommandID, '', True, 0, AParent);
  Adapter.SetChecked(AItem, TdxPDFViewerAccess(Viewer).OptionsNavigationPane.Bookmarks.TextSize = btsMedium);
  AItem := AddMenuItem(GetLargeTextSizeCaption, SetLargeTextSizeCommandID, '', True, 0, AParent);
  Adapter.SetChecked(AItem, TdxPDFViewerAccess(Viewer).OptionsNavigationPane.Bookmarks.TextSize = btsLarge);

  AddSeparator;
  inherited PopulateMenuItems;
end;

procedure TdxPDFViewerBookmarksPageOptionsPopupMenu.ProcessClick(ACommand, AParam: Word);
begin
  case ACommand of
    ExpandCurrentBookmarkCommandID:
      Bookmarks.ExpandCurrentBookmark;
    CollapseTopLevelBookmarksCommandID:
      Bookmarks.ExpandCollapseTopLevelBookmarks;
    HideAfterUseCommandID:
      TdxPDFViewerAccess(Viewer).OptionsNavigationPane.Bookmarks.HideAfterUse :=
        not TdxPDFViewerAccess(Viewer).OptionsNavigationPane.Bookmarks.HideAfterUse;
    SetSmallTextSizeCommandID:
      TdxPDFViewerAccess(Viewer).OptionsNavigationPane.Bookmarks.TextSize := btsSmall;
    SetMediumTextSizeCommandID:
      TdxPDFViewerAccess(Viewer).OptionsNavigationPane.Bookmarks.TextSize := btsMedium;
    SetLargeTextSizeCommandID:
      TdxPDFViewerAccess(Viewer).OptionsNavigationPane.Bookmarks.TextSize := btsLarge;
  else
    inherited ProcessClick(ACommand, AParam);
  end;
end;

function TdxPDFViewerBookmarksPageOptionsPopupMenu.GetCollapseTopLevelBookmarksCaption: Pointer;
begin
  if TdxPDFViewerBookmarksAccess(Bookmarks).IsTopLevelBookmarksExpanded then
    Result := @sdxPDFViewerBookmarksOptionsPopupMenuCollapseTopLevelBookmarks
  else
    Result := @sdxPDFViewerBookmarksOptionsPopupMenuExpandTopLevelBookmarks;
end;

function TdxPDFViewerBookmarksPageOptionsPopupMenu.GetExpandCurrentBookmarkCaption: Pointer;
begin
  Result := @sdxPDFViewerBookmarksOptionsPopupMenuExpandCurrentBookmark;
end;

function TdxPDFViewerBookmarksPageOptionsPopupMenu.GetHideAfterUseCaption: Pointer;
begin
  Result := @sdxPDFViewerBookmarksOptionsPopupMenuHideAfterUse;
end;

function TdxPDFViewerBookmarksPageOptionsPopupMenu.GetLargeTextSizeCaption: Pointer;
begin
  Result := @sdxPDFViewerBookmarksOptionsPopupMenuLargeTextSize;
end;

function TdxPDFViewerBookmarksPageOptionsPopupMenu.GetMediumTextSizeCaption: Pointer;
begin
  Result := @sdxPDFViewerBookmarksOptionsPopupMenuMediumTextSize;
end;

function TdxPDFViewerBookmarksPageOptionsPopupMenu.GetSmallTextSizeCaption: Pointer;
begin
  Result := @sdxPDFViewerBookmarksOptionsPopupMenuSmallTextSize;
end;

function TdxPDFViewerBookmarksPageOptionsPopupMenu.GetTextSizeCaption: Pointer;
begin
  Result := @sdxPDFViewerBookmarksOptionsPopupMenuTextSize;
end;

function TdxPDFViewerBookmarksPageOptionsPopupMenu.GetBookmarks: TdxPDFViewerBookmarks;
begin
  Result := TdxPDFViewerAccess(Viewer).Bookmarks;
end;

{ TdxPDFViewerThumbnailsPopupMenu }

function TdxPDFViewerThumbnailsPopupMenu.GetViewer: TdxPDFCustomViewer;
begin
  Result := Owner as TdxPDFCustomViewer;
end;

procedure TdxPDFViewerThumbnailsPopupMenu.PopulateMenuItems;
var
  AThumbnails: TdxPDFViewerThumbnailsAccess;
begin
  AThumbnails := TdxPDFViewerThumbnailsAccess(Thumbnails);
  if TdxPDFViewerAccess(Viewer).CanPrint then
  begin
    AddMenuItem(GetPrintPagesCaption, PrintPagesCommandID);
    AddSeparator;
  end;
  AddMenuItem(GetEnlargePageThumbnailsCaption, EnlargePageThumbnailsID, '', AThumbnails.CanEnlargePageThumbnails);
  AddMenuItem(GetReducePageThumbnailsCaption, ReducePageThumbnailsID, '', AThumbnails.CanReducePageThumbnails);
end;

procedure TdxPDFViewerThumbnailsPopupMenu.ProcessClick(ACommand, AParam: Word);
begin
  case ACommand of
    PrintPagesCommandID:
      TdxPDFViewerAccess(Viewer).Thumbnails.PrintPages;
    EnlargePageThumbnailsID:
      TdxPDFViewerAccess(Viewer).Thumbnails.EnlargePageThumbnails;
    ReducePageThumbnailsID:
      TdxPDFViewerAccess(Viewer).Thumbnails.ReducePageThumbnails;
  end;
end;

function TdxPDFViewerThumbnailsPopupMenu.GetEnlargePageThumbnailsCaption: Pointer;
begin
  Result := @sdxPDFViewerThumbnailPopupMenuEnlargePageThumbnails;
end;

function TdxPDFViewerThumbnailsPopupMenu.GetPrintPagesCaption: Pointer;
begin
  Result := @sdxPDFViewerThumbnailPopupMenuPrintPages;
end;

function TdxPDFViewerThumbnailsPopupMenu.GetReducePageThumbnailsCaption: Pointer;
begin
  Result := @sdxPDFViewerThumbnailPopupMenuReducePageThumbnails;
end;

function TdxPDFViewerThumbnailsPopupMenu.GetThumbnails: TdxPDFViewerThumbnails;
begin
  Result := TdxPDFViewerAccess(Viewer).Thumbnails;
end;

end.
