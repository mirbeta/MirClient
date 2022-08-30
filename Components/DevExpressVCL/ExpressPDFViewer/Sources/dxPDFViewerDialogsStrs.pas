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

unit dxPDFViewerDialogsStrs;

{$I cxVer.Inc}

interface

uses
  dxCore;

resourcestring
  sdxPDFViewerDocumentPropertiesDialogAdvanced = 'Advanced';
  sdxPDFViewerDocumentPropertiesDialogApplication = 'Application:';
  sdxPDFViewerDocumentPropertiesDialogAuthor = 'Author:';
  sdxPDFViewerDocumentPropertiesDialogCaption = 'Document Properties';
  sdxPDFViewerDocumentPropertiesDialogCreated = 'Created:';
  sdxPDFViewerDocumentPropertiesDialogDescription = 'Description';
  sdxPDFViewerDocumentPropertiesDialogFile = 'File:';
  sdxPDFViewerDocumentPropertiesDialogFileSize = 'File Size:';
  sdxPDFViewerDocumentPropertiesDialogKeywords = 'Keywords:';
  sdxPDFViewerDocumentPropertiesDialogLocation = 'Location:';
  sdxPDFViewerDocumentPropertiesDialogModified = 'Modified:';
  sdxPDFViewerDocumentPropertiesDialogNumberOfPages = 'Number of Pages:';
  sdxPDFViewerDocumentPropertiesDialogPageSize = 'Page Size:';
  sdxPDFViewerDocumentPropertiesDialogProducer = 'Producer:';
  sdxPDFViewerDocumentPropertiesDialogRevision = 'Revision';
  sdxPDFViewerDocumentPropertiesDialogSubject = 'Subject:';
  sdxPDFViewerDocumentPropertiesDialogTitle = 'Title:';
  sdxPDFViewerDocumentPropertiesDialogVersion = 'Version:';

  sdxPDFViewerPasswordDialogCaption = 'Enter Password';
  sdxPDFViewerPasswordDialogProtectedDocument = 'This document is password protected. Enter a valid user or owner password to open it.';
  sdxPDFViewerPasswordDialogPassword = 'Password:';
  sdxPDFViewerPasswordDialogButtonCancel = 'Cancel';
  sdxPDFViewerPasswordDialogButtonOK = 'OK';

  sdxPDFViewerFindPanelFindCaption = 'Search';
  sdxPDFViewerFindPanelNextButtonCaption = 'Next';
  sdxPDFViewerFindPanelPreviousButtonCaption = 'Previous';
  sdxPDFViewerTextSearchingNoMatchesFoundMessage = 'Finished searching the document. No matches were found.';
  sdxPDFViewerTextSearchingCompleteMessage = 'Finished searching the document. No more matches were found.';

  sdxPDFViewerPopupMenuCopyImage = 'Copy Image';
  sdxPDFViewerPopupMenuCopyText = 'Copy';
  sdxPDFViewerPopupMenuDocumentProperties = 'Document Properties...';
  sdxPDFViewerPopupMenuPrint = 'Print...';

  sdxPDFViewerPopupMenuOpenAttachmentFileText = 'Open File';
  sdxPDFViewerPopupMenuSaveAttachmentFileText = 'Save Embedded File to Disk...';

  sdxPDFViewerFindPanelPopupMenuCaseSensitive = 'Case Sensitive';
  sdxPDFViewerFindPanelPopupMenuWholeWords = 'Whole Words Only';

  sdxPDFViewerBookmarkPopupMenuGoToBookmark = 'Go to Bookmark';
  sdxPDFViewerBookmarkPopupMenuPrintPages = 'Print Page(s)';
  sdxPDFViewerBookmarkPopupMenuPrintSections = 'Print Section(s)';
  sdxPDFViewerBookmarksOptionsPopupMenuCollapseTopLevelBookmarks = 'Collapse Top-Level Bookmarks';
  sdxPDFViewerBookmarksOptionsPopupMenuExpandCurrentBookmark = 'Expand Current Bookmark';
  sdxPDFViewerBookmarksOptionsPopupMenuExpandTopLevelBookmarks = 'Expand Top-Level Bookmarks';
  sdxPDFViewerBookmarksOptionsPopupMenuHideAfterUse = 'Hide After Use';
  sdxPDFViewerBookmarksOptionsPopupMenuLargeTextSize = 'Large';
  sdxPDFViewerBookmarksOptionsPopupMenuMediumTextSize = 'Medium';
  sdxPDFViewerBookmarksOptionsPopupMenuSmallTextSize = 'Small';
  sdxPDFViewerBookmarksOptionsPopupMenuTextSize = 'Text Size';

  sdxPDFViewerNavigationPageAttachmentDescriptionCaption = 'Description: ';
  sdxPDFViewerNavigationPageAttachmentFileNameCaption = 'Name: ';
  sdxPDFViewerNavigationPageAttachmentFileSizeCaption = 'Size: ';
  sdxPDFViewerNavigationPageAttachmentModifiedCaption = 'Modified: ';
  sdxPDFViewerNavigationPageAttachmentsCaption = 'Attachments';
  sdxPDFViewerNavigationPageOpenAttachmentButtonHint = 'Open file in its native application';
  sdxPDFViewerNavigationPageSaveAttachmentButtonHint = 'Save attachment';

  sdxPDFViewerNavigationPageBookmarksCaption = 'Bookmarks';
  sdxPDFViewerNavigationPageThumbnailsCaption = 'Page Thumbnails';
  sdxPDFViewerNavigationPageExpandBookmarkButtonHint = 'Expand current bookmark';
  sdxPDFViewerNavigationPageOptionsButtonHint = 'Options';
  sdxPDFViewerNavigationPageThumbnailsSizeTrackBarHint = 'Zoom page thumbnails';
  sdxPDFViewerNavigationPageExpandButtonHint = 'Expand';
  sdxPDFViewerNavigationPageCollapseButtonHint = 'Collapse';
  sdxPDFViewerNavigationPageHideButtonHint = 'Hide';
  sdxPDFViewerThumbnailPopupMenuEnlargePageThumbnails = 'Enlarge Page Thumbnails';
  sdxPDFViewerThumbnailPopupMenuPrintPages = 'Print Page(s)';
  sdxPDFViewerThumbnailPopupMenuReducePageThumbnails = 'Reduce Page Thumbnails';

  sdxPDFViewerBytes = 'Bytes';
  sdxPDFViewerKiloBytes = 'KB';
  sdxPDFViewerMegaBytes = 'MB';
  sdxPDFViewerGigaBytes = 'GB';
  sdxPDFViewerUnitsInches = 'in';

implementation

procedure AddPDFViewerDialogsResourceStrings(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogAdvanced', @sdxPDFViewerDocumentPropertiesDialogAdvanced);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogApplication', @sdxPDFViewerDocumentPropertiesDialogApplication);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogAuthor', @sdxPDFViewerDocumentPropertiesDialogAuthor);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogCaption', @sdxPDFViewerDocumentPropertiesDialogCaption);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogCreated', @sdxPDFViewerDocumentPropertiesDialogCreated);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogDescription', @sdxPDFViewerDocumentPropertiesDialogDescription);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogFile', @sdxPDFViewerDocumentPropertiesDialogFile);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogFileSize', @sdxPDFViewerDocumentPropertiesDialogFileSize);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogKeywords', @sdxPDFViewerDocumentPropertiesDialogKeywords);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogLocation', @sdxPDFViewerDocumentPropertiesDialogLocation);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogModified', @sdxPDFViewerDocumentPropertiesDialogModified);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogNumberOfPages', @sdxPDFViewerDocumentPropertiesDialogNumberOfPages);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogPageSize', @sdxPDFViewerDocumentPropertiesDialogPageSize);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogProducer', @sdxPDFViewerDocumentPropertiesDialogProducer);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogRevision', @sdxPDFViewerDocumentPropertiesDialogRevision);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogSubject', @sdxPDFViewerDocumentPropertiesDialogSubject);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogTitle', @sdxPDFViewerDocumentPropertiesDialogTitle);
  AProduct.Add('sdxPDFViewerDocumentPropertiesDialogVersion', @sdxPDFViewerDocumentPropertiesDialogVersion);

  AProduct.Add('sdxPDFViewerPasswordDialogCaption', @sdxPDFViewerPasswordDialogCaption);
  AProduct.Add('sdxPDFViewerPasswordDialogProtectedDocument', @sdxPDFViewerPasswordDialogProtectedDocument);
  AProduct.Add('sdxPDFViewerPasswordDialogPassword', @sdxPDFViewerPasswordDialogPassword);
  AProduct.Add('sdxPDFViewerPasswordDialogButtonCancel', @sdxPDFViewerPasswordDialogButtonCancel);
  AProduct.Add('sdxPDFViewerPasswordDialogButtonOK', @sdxPDFViewerPasswordDialogButtonOK);

  AProduct.Add('sdxPDFViewerPopupMenuCopyImage', @sdxPDFViewerPopupMenuCopyImage);
  AProduct.Add('sdxPDFViewerPopupMenuCopyText', @sdxPDFViewerPopupMenuCopyText);
  AProduct.Add('sdxPDFViewerPopupMenuDocumentProperties', @sdxPDFViewerPopupMenuDocumentProperties);
  AProduct.Add('sdxPDFViewerPopupMenuPrint', @sdxPDFViewerPopupMenuPrint);

  AProduct.Add('sdxPDFViewerPopupMenuOpenAttachmentFileText', @sdxPDFViewerPopupMenuOpenAttachmentFileText);
  AProduct.Add('sdxPDFViewerPopupMenuSaveAttachmentFileText', @sdxPDFViewerPopupMenuSaveAttachmentFileText);

  AProduct.Add('sdxPDFViewerFindPanelFindCaption', @sdxPDFViewerFindPanelFindCaption);
  AProduct.Add('sdxPDFViewerFindPanelNextButtonCaption', @sdxPDFViewerFindPanelNextButtonCaption);
  AProduct.Add('sdxPDFViewerFindPanelPreviousButtonCaption', @sdxPDFViewerFindPanelPreviousButtonCaption);
  AProduct.Add('sdxPDFViewerTextSearchingNoMatchesFoundMessage', @sdxPDFViewerTextSearchingNoMatchesFoundMessage);
  AProduct.Add('sdxPDFViewerTextSearchingCompleteMessage', @sdxPDFViewerTextSearchingCompleteMessage);

  AProduct.Add('sdxPDFViewerFindPanelPopupMenuCaseSensitive', @sdxPDFViewerFindPanelPopupMenuCaseSensitive);
  AProduct.Add('sdxPDFViewerFindPanelPopupMenuWholeWords', @sdxPDFViewerFindPanelPopupMenuWholeWords);

  AProduct.Add('sdxPDFViewerBookmarkPopupMenuGoToBookmark', @sdxPDFViewerBookmarkPopupMenuGoToBookmark);
  AProduct.Add('sdxPDFViewerBookmarkPopupMenuPrintPages', @sdxPDFViewerBookmarkPopupMenuPrintPages);
  AProduct.Add('sdxPDFViewerBookmarkPopupMenuPrintSections', @sdxPDFViewerBookmarkPopupMenuPrintSections);
  AProduct.Add('sdxPDFViewerBookmarksOptionsPopupMenuCollapseTopLevelBookmarks', @sdxPDFViewerBookmarksOptionsPopupMenuCollapseTopLevelBookmarks);
  AProduct.Add('sdxPDFViewerBookmarksOptionsPopupMenuExpandCurrentBookmark', @sdxPDFViewerBookmarksOptionsPopupMenuExpandCurrentBookmark);
  AProduct.Add('sdxPDFViewerBookmarksOptionsPopupMenuExpandTopLevelBookmarks', @sdxPDFViewerBookmarksOptionsPopupMenuExpandTopLevelBookmarks);
  AProduct.Add('sdxPDFViewerBookmarksOptionsPopupMenuHideAfterUse', @sdxPDFViewerBookmarksOptionsPopupMenuHideAfterUse);
  AProduct.Add('sdxPDFViewerBookmarksOptionsPopupMenuLargeTextSize', @sdxPDFViewerBookmarksOptionsPopupMenuLargeTextSize);
  AProduct.Add('sdxPDFViewerBookmarksOptionsPopupMenuMediumTextSize', @sdxPDFViewerBookmarksOptionsPopupMenuMediumTextSize);
  AProduct.Add('sdxPDFViewerBookmarksOptionsPopupMenuSmallTextSize', @sdxPDFViewerBookmarksOptionsPopupMenuSmallTextSize);
  AProduct.Add('sdxPDFViewerBookmarksOptionsPopupMenuTextSize', @sdxPDFViewerBookmarksOptionsPopupMenuTextSize);

  AProduct.Add('sdxPDFViewerNavigationPageAttachmentDescriptionCaption', @sdxPDFViewerNavigationPageAttachmentDescriptionCaption);
  AProduct.Add('sdxPDFViewerNavigationPageAttachmentFileNameCaption', @sdxPDFViewerNavigationPageAttachmentFileNameCaption);
  AProduct.Add('sdxPDFViewerNavigationPageAttachmentFileSizeCaption', @sdxPDFViewerNavigationPageAttachmentFileSizeCaption);
  AProduct.Add('sdxPDFViewerNavigationPageAttachmentModifiedCaption', @sdxPDFViewerNavigationPageAttachmentModifiedCaption);
  AProduct.Add('sdxPDFViewerNavigationPageAttachmentsCaption', @sdxPDFViewerNavigationPageAttachmentsCaption);
  AProduct.Add('sdxPDFViewerNavigationPageOpenAttachmentButtonHint', @sdxPDFViewerNavigationPageOpenAttachmentButtonHint);
  AProduct.Add('sdxPDFViewerNavigationPageSaveAttachmentButtonHint', @sdxPDFViewerNavigationPageSaveAttachmentButtonHint);

  AProduct.Add('sdxPDFViewerNavigationPageBookmarksCaption', @sdxPDFViewerNavigationPageBookmarksCaption);
  AProduct.Add('sdxPDFViewerNavigationPageExpandBookmarkButtonHint', @sdxPDFViewerNavigationPageExpandBookmarkButtonHint);
  AProduct.Add('sdxPDFViewerNavigationPageOptionsButtonHint', @sdxPDFViewerNavigationPageOptionsButtonHint);
  AProduct.Add('sdxPDFViewerNavigationPageExpandButtonHint', @sdxPDFViewerNavigationPageExpandButtonHint);
  AProduct.Add('sdxPDFViewerNavigationPageCollapseButtonHint', @sdxPDFViewerNavigationPageCollapseButtonHint);
  AProduct.Add('sdxPDFViewerNavigationPageHideButtonHint', @sdxPDFViewerNavigationPageHideButtonHint);
  AProduct.Add('sdxPDFViewerNavigationPageThumbnailsCaption', @sdxPDFViewerNavigationPageThumbnailsCaption);
  AProduct.Add('sdxPDFViewerNavigationPageThumbnailsSizeTrackBarHint', @sdxPDFViewerNavigationPageThumbnailsSizeTrackBarHint);
  AProduct.Add('sdxPDFViewerThumbnailPopupMenuEnlargePageThumbnails', @sdxPDFViewerThumbnailPopupMenuEnlargePageThumbnails);
  AProduct.Add('sdxPDFViewerThumbnailPopupMenuPrintPages', @sdxPDFViewerThumbnailPopupMenuPrintPages);
  AProduct.Add('sdxPDFViewerThumbnailPopupMenuReducePageThumbnails', @sdxPDFViewerThumbnailPopupMenuReducePageThumbnails);

  AProduct.Add('sdxPDFViewerBytes', @sdxPDFViewerBytes);
  AProduct.Add('sdxPDFViewerKiloBytes', @sdxPDFViewerKiloBytes);
  AProduct.Add('sdxPDFViewerMegaBytes', @sdxPDFViewerMegaBytes);
  AProduct.Add('sdxPDFViewerGigaBytes', @sdxPDFViewerGigaBytes);
  AProduct.Add('sdxPDFViewerUnitsInches', @sdxPDFViewerUnitsInches);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressPDFViewer', @AddPDFViewerDialogsResourceStrings);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressPDFViewer', @AddPDFViewerDialogsResourceStrings);
end.

