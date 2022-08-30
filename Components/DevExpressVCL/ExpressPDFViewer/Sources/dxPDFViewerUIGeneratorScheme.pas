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

unit dxPDFViewerUIGeneratorScheme;

{$I cxVer.Inc}

interface

const
  sdxPDFViewerActionsCategoryName = 'DevExpress ExpressPDFViewer';

  sdxPDFViewerTabPDFViewer = 'Home';
  sdxPDFViewerBarPDFViewerFile = 'File';
  sdxPDFViewerBarPDFViewerFind = 'Find';
  sdxPDFViewerBarPDFViewerNavigation = 'Navigation';
  sdxPDFViewerBarPDFViewerZoom = 'Zoom';
  sdxPDFViewerBarPDFViewerTools = 'Tools';

procedure RegisterPDFViewerUIGeneratorScheme;

implementation

uses
  dxUIGenerator, dxPDFViewer, dxPDFViewerActions, dxPDFViewerActionsStrs;

procedure RegisterCategoryFile(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxPDFViewerOpenDocument);
  ACategory.Add(TdxPDFViewerCloseDocument);
end;

procedure RegisterCategoryFind(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxPDFViewerFind);
end;

procedure RegisterCategoryNavigation(ACategory: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACommand := ACategory.Add('Arrows\DoubleLast.png', sdxPDFViewerActionNavigationCaption);
  ACommand.BeginGroup := True;
  ACommand.Add(TdxPDFViewerGoToFirstPage);
  ACommand.Add(TdxPDFViewerGoToPrevPage);
  ACommand.Add(TdxPDFViewerGoToNextPage);
  ACommand.Add(TdxPDFViewerGoToLastPage);
  ACommand.Add(TdxPDFViewerGoToPrevView, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);
  ACommand.Add(TdxPDFViewerGoToNextView);

  ACommand := ACategory.Add('Actions\Refresh2.png', sdxPDFViewerActionRotateViewCaption);
  ACommand.Add(TdxPDFViewerRotateClockwise);
  ACommand.Add(TdxPDFViewerRotateCounterClockwise);
end;

procedure RegisterCategoryZoom(ACategory: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACategory.Add(TdxPDFViewerZoomOut);
  ACategory.Add(TdxPDFViewerZoomIn);

  ACommand := ACategory.Add('Zoom\Zoom.png', sdxPDFViewerActionZoomListCaption);
  ACommand.BeginGroup := True;
  ACommand.Add(TdxPDFViewerZoom10);
  ACommand.Add(TdxPDFViewerZoom25);
  ACommand.Add(TdxPDFViewerZoom50);
  ACommand.Add(TdxPDFViewerZoom75);
  ACommand.Add(TdxPDFViewerZoom100);
  ACommand.Add(TdxPDFViewerZoom125);
  ACommand.Add(TdxPDFViewerZoom150);
  ACommand.Add(TdxPDFViewerZoom200);
  ACommand.Add(TdxPDFViewerZoom400);
  ACommand.Add(TdxPDFViewerZoom500);
  ACommand.Add(TdxPDFViewerZoomActualSize, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);
  ACommand.Add(TdxPDFViewerZoomToPageLevel);
  ACommand.Add(TdxPDFViewerZoomFitWidth);
end;

procedure RegisterCategoryTools(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxPDFViewerSelectTool);
  ACategory.Add(TdxPDFViewerHandTool);
  ACategory.Add(TdxPDFViewerSelectAll, dxUIGeneratorItemDefaultViewLevels, ugigpNone, ugipBeginsNewRow, True);
end;

procedure RegisterPDFViewerUIGeneratorScheme;
var
  AComponent: TdxUIGeneratorComponentInfo;
begin
  AComponent := TdxUIGenerator.RegisterComponent(TdxPDFViewer, sdxPDFViewerActionsCategoryName);
  RegisterCategoryFile(AComponent.Add(sdxPDFViewerTabPDFViewer, sdxPDFViewerBarPDFViewerFile, 'Actions\New_16x16.png', 0));
  RegisterCategoryFind(AComponent.Add(sdxPDFViewerTabPDFViewer, sdxPDFViewerBarPDFViewerFind, 'Find\Find_16x16.png', 0));
  RegisterCategoryTools(AComponent.Add(sdxPDFViewerTabPDFViewer, sdxPDFViewerBarPDFViewerTools, 'PDF Viewer\SelectTool_16x16.png', 0));
  RegisterCategoryNavigation(AComponent.Add(sdxPDFViewerTabPDFViewer, sdxPDFViewerBarPDFViewerNavigation,
    'Arrows\DoubleLast_16x16.png', 0));
  RegisterCategoryZoom(AComponent.Add(sdxPDFViewerTabPDFViewer, sdxPDFViewerBarPDFViewerZoom, 'Zoom\Zoom_16x16.png', 0));
end;

end.
