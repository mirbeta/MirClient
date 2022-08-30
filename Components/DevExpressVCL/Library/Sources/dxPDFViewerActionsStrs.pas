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

unit dxPDFViewerActionsStrs;

{$I cxVer.inc}

interface

resourcestring
  sdxPDFViewerActionCloseDocumentCaption = 'Close';
  sdxPDFViewerActionCloseDocumentHint = 'Close a document';
  sdxPDFViewerActionOpenDocumentCaption = 'Open';
  sdxPDFViewerActionOpenDocumentHint = 'Open a PDF file';
  sdxPDFViewerActionNavigationCaption = 'Navigation';
  sdxPDFViewerActionFindCaption = 'Find';
  sdxPDFViewerActionFindHint = 'Find text';
  sdxPDFViewerActionGoToNextPageCaption = 'Next Page';
  sdxPDFViewerActionGoToNextPageHint = 'Show next page';
  sdxPDFViewerActionGoToNextViewCaption = 'Next View';
  sdxPDFViewerActionGoToNextViewHint = 'Go to the next view';
  sdxPDFViewerActionGoToFirstPageCaption = 'First Page';
  sdxPDFViewerActionGoToFirstPageHint = 'Show first page';
  sdxPDFViewerActionGoToLastPageCaption = 'Last Page';
  sdxPDFViewerActionGoToLastPageHint = 'Show last page';
  sdxPDFViewerActionGoToPrevPageCaption = 'Previous Page';
  sdxPDFViewerActionGoToPrevPageHint = 'Show previous page';
  sdxPDFViewerActionGoToPrevViewCaption = 'Previous View';
  sdxPDFViewerActionGoToPrevViewHint = 'Return to the previous view';
  sdxPDFViewerActionHandToolCaption = 'Hand Tool';
  sdxPDFViewerActionPrintCaption = '&Print';
  sdxPDFViewerActionPrintHint = 'Print';
  sdxPDFViewerActionRotateViewCaption = 'Rotate View';
  sdxPDFViewerActionRotateClockwiseCaption = 'Rotate Clockwise';
  sdxPDFViewerActionRotateClockwiseHint = 'Rotate the current view clockwise';
  sdxPDFViewerActionRotateCounterclockwiseCaption = 'Rotate Counterclockwise';
  sdxPDFViewerActionRotateCounterclockwiseHint = 'Rotate the current view counterclockwise';
  sdxPDFViewerActionSelectAllCaption = 'Select All';
  sdxPDFViewerActionSelectAllHint = 'Select all text in a document';
  sdxPDFViewerActionSelectToolCaption = 'Select Tool';
  sdxPDFViewerActionZoomInCaption = 'Zoom In';
  sdxPDFViewerActionZoomInHint = 'Zoom in to get a close-up view of the PDF document';
  sdxPDFViewerActionZoomOutCaption = 'Zoom Out';
  sdxPDFViewerActionZoomOutHint = 'Zoom out to see more of the page at a reduced size';
  sdxPDFViewerActionZoomListCaption = 'Zoom';
  sdxPDFViewerActionZoomListHint = 'Change the zoom level of the PDF document';
  sdxPDFViewerActionZoom10Caption = '10%';
  sdxPDFViewerActionZoom25Caption = '25%';
  sdxPDFViewerActionZoom50Caption = '50%';
  sdxPDFViewerActionZoom75Caption = '75%';
  sdxPDFViewerActionZoom100Caption = '100%';
  sdxPDFViewerActionZoom125Caption = '125%';
  sdxPDFViewerActionZoom150Caption = '150%';
  sdxPDFViewerActionZoom200Caption = '200%';
  sdxPDFViewerActionZoom400Caption = '400%';
  sdxPDFViewerActionZoom500Caption = '500%';
  sdxPDFViewerActionZoomActualSizeCaption = 'Actual Size';
  sdxPDFViewerActionZoomToPageLevelCaption = 'Zoom to Page Level';
  sdxPDFViewerActionZoomFitWidthCaption = 'Fit Width';

implementation

uses
  dxCore;

procedure AddPDFViewerActionsResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxPDFViewerActionCloseDocumentCaption', @sdxPDFViewerActionCloseDocumentCaption);
  AProduct.Add('sdxPDFViewerActionCloseDocumentHint', @sdxPDFViewerActionCloseDocumentHint);
  AProduct.Add('sdxPDFViewerActionOpenDocumentCaption', @sdxPDFViewerActionOpenDocumentCaption);
  AProduct.Add('sdxPDFViewerActionOpenDocumentHint', @sdxPDFViewerActionOpenDocumentHint);
  AProduct.Add('sdxPDFViewerActionNavigationCaption', @sdxPDFViewerActionNavigationCaption);
  AProduct.Add('sdxPDFViewerActionFindCaption', @sdxPDFViewerActionFindCaption);
  AProduct.Add('sdxPDFViewerActionFindHint', @sdxPDFViewerActionFindHint);
  AProduct.Add('sdxPDFViewerActionGoToFirstPageCaption', @sdxPDFViewerActionGoToFirstPageCaption);
  AProduct.Add('sdxPDFViewerActionGoToFirstPageHint', @sdxPDFViewerActionGoToFirstPageHint);
  AProduct.Add('sdxPDFViewerActionGoToNextPageCaption', @sdxPDFViewerActionGoToNextPageCaption);
  AProduct.Add('sdxPDFViewerActionGoToNextPageHint', @sdxPDFViewerActionGoToNextPageHint);
  AProduct.Add('sdxPDFViewerActionGoToNextViewCaption', @sdxPDFViewerActionGoToNextViewCaption);
  AProduct.Add('sdxPDFViewerActionGoToNextViewHint', @sdxPDFViewerActionGoToNextViewHint);
  AProduct.Add('sdxPDFViewerActionGoToPrevPageCaption', @sdxPDFViewerActionGoToPrevPageCaption);
  AProduct.Add('sdxPDFViewerActionGoToPrevPageHint', @sdxPDFViewerActionGoToPrevPageHint);
  AProduct.Add('sdxPDFViewerActionGoToPrevViewCaption', @sdxPDFViewerActionGoToPrevViewCaption);
  AProduct.Add('sdxPDFViewerActionGoToPrevViewHint', @sdxPDFViewerActionGoToPrevViewHint);
  AProduct.Add('sdxPDFViewerActionGoToLastPageCaption', @sdxPDFViewerActionGoToLastPageCaption);
  AProduct.Add('sdxPDFViewerActionGoToLastPageHint', @sdxPDFViewerActionGoToLastPageHint);
  AProduct.Add('sdxPDFViewerActionHandToolCaption', @sdxPDFViewerActionHandToolCaption);
  AProduct.Add('sdxPDFViewerActionRotateClockwiseCaption', @sdxPDFViewerActionRotateClockwiseCaption);
  AProduct.Add('sdxPDFViewerActionRotateClockwiseHint', @sdxPDFViewerActionRotateClockwiseHint);
  AProduct.Add('sdxPDFViewerActionRotateCounterclockwiseCaption', @sdxPDFViewerActionRotateCounterclockwiseCaption);
  AProduct.Add('sdxPDFViewerActionRotateCounterclockwiseHint', @sdxPDFViewerActionRotateCounterclockwiseHint);
  AProduct.Add('sdxPDFViewerActionRotateViewCaption', @sdxPDFViewerActionRotateViewCaption);
  AProduct.Add('sdxPDFViewerActionSelectAllCaption', @sdxPDFViewerActionSelectAllCaption);
  AProduct.Add('sdxPDFViewerActionSelectAllHint', @sdxPDFViewerActionSelectAllHint);
  AProduct.Add('sdxPDFViewerActionPrintCaption', @sdxPDFViewerActionPrintCaption);
  AProduct.Add('sdxPDFViewerActionPrintHint', @sdxPDFViewerActionPrintHint);
  AProduct.Add('sdxPDFViewerActionSelectToolCaption', @sdxPDFViewerActionSelectToolCaption);
  AProduct.Add('sdxPDFViewerActionZoomInCaption', @sdxPDFViewerActionZoomInCaption);
  AProduct.Add('sdxPDFViewerActionZoomInHint', @sdxPDFViewerActionZoomInHint);
  AProduct.Add('sdxPDFViewerActionZoomOutCaption', @sdxPDFViewerActionZoomOutCaption);
  AProduct.Add('sdxPDFViewerActionZoomOutHint', @sdxPDFViewerActionZoomOutHint);
  AProduct.Add('sdxPDFViewerActionZoomListCaption', @sdxPDFViewerActionZoomListCaption);
  AProduct.Add('sdxPDFViewerActionZoomListHint', @sdxPDFViewerActionZoomListHint);
  AProduct.Add('sdxPDFViewerActionZoom10Caption', @sdxPDFViewerActionZoom10Caption);
  AProduct.Add('sdxPDFViewerActionZoom25Caption', @sdxPDFViewerActionZoom25Caption);
  AProduct.Add('sdxPDFViewerActionZoom50Caption', @sdxPDFViewerActionZoom50Caption);
  AProduct.Add('sdxPDFViewerActionZoom75Caption', @sdxPDFViewerActionZoom75Caption);
  AProduct.Add('sdxPDFViewerActionZoom100Caption', @sdxPDFViewerActionZoom100Caption);
  AProduct.Add('sdxPDFViewerActionZoom125Caption', @sdxPDFViewerActionZoom125Caption);
  AProduct.Add('sdxPDFViewerActionZoom150Caption', @sdxPDFViewerActionZoom150Caption);
  AProduct.Add('sdxPDFViewerActionZoom200Caption', @sdxPDFViewerActionZoom200Caption);
  AProduct.Add('sdxPDFViewerActionZoom400Caption', @sdxPDFViewerActionZoom400Caption);
  AProduct.Add('sdxPDFViewerActionZoom500Caption', @sdxPDFViewerActionZoom500Caption);
  AProduct.Add('sdxPDFViewerActionZoomActualSizeCaption', @sdxPDFViewerActionZoomActualSizeCaption);
  AProduct.Add('sdxPDFViewerActionZoomToPageLevelCaption', @sdxPDFViewerActionZoomToPageLevelCaption);
  AProduct.Add('sdxPDFViewerActionZoomFitWidthCaption', @sdxPDFViewerActionZoomFitWidthCaption);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressPDFViewer', @AddPDFViewerActionsResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressPDFViewer', @AddPDFViewerActionsResourceStringNames);

end.
