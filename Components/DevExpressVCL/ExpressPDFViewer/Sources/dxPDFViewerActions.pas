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

unit dxPDFViewerActions;

{$I cxVer.Inc}

interface

uses
  Types, Classes, Controls, dxActions, dxPDFViewer, dxPrinting;

type
  { TdxPDFViewerCustomAction }

  TdxPDFViewerCustomAction = class(TdxCustomAction)
  strict private
    function GetControl: TdxPDFViewer;
  protected
    procedure DoUpdateState; override;
    procedure DoResetState; override;
    procedure UpdateControl(Target: TObject); override;

    function IsEnabled: Boolean; virtual;
    procedure DoExecute; virtual;
    procedure SetControl(AValue: TdxPDFViewer); reintroduce;

    property Control: TdxPDFViewer read GetControl write SetControl;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  { TdxPDFViewerCheckableAction }

  TdxPDFViewerCheckableAction = class(TdxPDFViewerCustomAction)
  protected
    procedure DoUpdateState; override;
    procedure DoResetState; override;

    function IsChecked: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerOpenDocument }

  TdxPDFViewerOpenDocument = class(TdxPDFViewerCustomAction)
  protected
    function IsEnabled: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerCloseDocument }

  TdxPDFViewerCloseDocument = class(TdxPDFViewerCustomAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerNavigationAction }

  TdxPDFViewerNavigationAction = class(TdxPDFViewerCustomAction);

  { TdxPDFViewerGoToNextPage }

  TdxPDFViewerGoToNextPage = class(TdxPDFViewerNavigationAction)
  protected
    function IsEnabled: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerGoToFirstPage }

  TdxPDFViewerGoToFirstPage = class(TdxPDFViewerNavigationAction)
  protected
    function IsEnabled: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerGoToLastPage }

  TdxPDFViewerGoToLastPage = class(TdxPDFViewerNavigationAction)
  protected
    function IsEnabled: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerGoToPrevPage }

  TdxPDFViewerGoToPrevPage = class(TdxPDFViewerNavigationAction)
  protected
    function IsEnabled: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerZoomIn }

  TdxPDFViewerZoomIn = class(TdxPDFViewerCustomAction)
  protected
    function IsEnabled: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerZoomOut }

  TdxPDFViewerZoomOut = class(TdxPDFViewerCustomAction)
  protected
    function IsEnabled: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerFixedZoomAction }

  TdxPDFViewerFixedZoomAction = class(TdxPDFViewerCheckableAction)
  protected
    function IsChecked: Boolean; override;
    procedure DoExecute; override;

    function GetFixedZoomActionCaption: Pointer; virtual;
    function GetFixedZoomValue: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerZoom10 }

  TdxPDFViewerZoom10 = class(TdxPDFViewerFixedZoomAction)
  protected
    function GetFixedZoomActionCaption: Pointer; override;
    function GetFixedZoomValue: Integer; override;
  end;

  { TdxPDFViewerZoom25 }

  TdxPDFViewerZoom25 = class(TdxPDFViewerFixedZoomAction)
  protected
    function GetFixedZoomActionCaption: Pointer; override;
    function GetFixedZoomValue: Integer; override;
  end;

  { TdxPDFViewerZoom50 }

  TdxPDFViewerZoom50 = class(TdxPDFViewerFixedZoomAction)
  protected
    function GetFixedZoomActionCaption: Pointer; override;
    function GetFixedZoomValue: Integer; override;
  end;

  { TdxPDFViewerZoom75 }

  TdxPDFViewerZoom75 = class(TdxPDFViewerFixedZoomAction)
  protected
    function GetFixedZoomActionCaption: Pointer; override;
    function GetFixedZoomValue: Integer; override;
  end;

  { TdxPDFViewerZoom100 }

  TdxPDFViewerZoom100 = class(TdxPDFViewerFixedZoomAction)
  protected
    function GetFixedZoomActionCaption: Pointer; override;
  end;

  { TdxPDFViewerZoom125 }

  TdxPDFViewerZoom125 = class(TdxPDFViewerFixedZoomAction)
  protected
    function GetFixedZoomActionCaption: Pointer; override;
    function GetFixedZoomValue: Integer; override;
  end;

  { TdxPDFViewerZoom150 }

  TdxPDFViewerZoom150 = class(TdxPDFViewerFixedZoomAction)
  protected
    function GetFixedZoomActionCaption: Pointer; override;
    function GetFixedZoomValue: Integer; override;
  end;

  { TdxPDFViewerZoom200 }

  TdxPDFViewerZoom200 = class(TdxPDFViewerFixedZoomAction)
  protected
    function GetFixedZoomActionCaption: Pointer; override;
    function GetFixedZoomValue: Integer; override;
  end;

  { TdxPDFViewerZoom400 }

  TdxPDFViewerZoom400 = class(TdxPDFViewerFixedZoomAction)
  protected
    function GetFixedZoomActionCaption: Pointer; override;
    function GetFixedZoomValue: Integer; override;
  end;

  { TdxPDFViewerZoom500 }

  TdxPDFViewerZoom500 = class(TdxPDFViewerFixedZoomAction)
  protected
    function GetFixedZoomActionCaption: Pointer; override;
    function GetFixedZoomValue: Integer; override;
  end;

  { TdxPDFViewerZoomActualSize }

  TdxPDFViewerZoomActualSize = class(TdxPDFViewerZoom100)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerZoomToPageLevel }

  TdxPDFViewerZoomToPageLevel = class(TdxPDFViewerCheckableAction)
  protected
    function IsChecked: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerZoomFitWidth }

  TdxPDFViewerZoomFitWidth = class(TdxPDFViewerCheckableAction)
  protected
    function IsChecked: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerRotateClockwise }

  TdxPDFViewerRotateClockwise = class(TdxPDFViewerCustomAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerRotateCounterClockwise }

  TdxPDFViewerRotateCounterClockwise = class(TdxPDFViewerCustomAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerGoToNextView }

  TdxPDFViewerGoToNextView = class(TdxPDFViewerCustomAction)
  protected
    function IsEnabled: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerGoToPrevView }

  TdxPDFViewerGoToPrevView = class(TdxPDFViewerCustomAction)
  protected
    function IsEnabled: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerSelectTool }

  TdxPDFViewerSelectTool = class(TdxPDFViewerCheckableAction)
  protected
    function IsChecked: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerHandTool }

  TdxPDFViewerHandTool = class(TdxPDFViewerCheckableAction)
  protected
    function IsChecked: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerSelectAll }

  TdxPDFViewerSelectAll = class(TdxPDFViewerCustomAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerShowPrintForm }

  TdxPDFViewerShowPrintForm = class(TdxCustomShowPrintFormAction)
  protected
    function IsEnabled: Boolean; override;
    function GetControlClass: TWinControlClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxPDFViewerFind }

  TdxPDFViewerFind = class(TdxPDFViewerCheckableAction)
  protected
    function IsChecked: Boolean; override;
    function IsEnabled: Boolean; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

function dxPDFViewerOpenDocumentDialog(var AFileName: string): Boolean;

implementation

uses
  Dialogs, dxPDFViewerActionsStrs, dxCustomPreview;

type
  TdxPDFViewerAccess = class(TdxPDFCustomViewer);
  TdxPDFViewerOptionsFindPanelAccess = class(TdxPDFViewerOptionsFindPanel);

function dxPDFViewerOpenDocumentDialog(var AFileName: string): Boolean;
var
  AOpenDialog: TOpenDialog;
begin
  AOpenDialog := TOpenDialog.Create(nil);
  AOpenDialog.Filter := 'PDF Document (*.pdf)|*.pdf;';
  try
    Result := AOpenDialog.Execute;
    if Result then
      AFileName := AOpenDialog.FileName
    else
      AFileName := '';
  finally
    AOpenDialog.Free;
  end;
end;

{ TdxPDFViewerCustomAction }

procedure TdxPDFViewerCustomAction.ExecuteTarget(Target: TObject);
begin
  UpdateControl(Target);
  if Control <> nil then
    DoExecute;
end;

function TdxPDFViewerCustomAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (inherited HandlesTarget(Target) or (Target is TdxPDFCustomViewer)) and
    (not NeedControlFocus or TdxPDFCustomViewer(Target).Focused);
end;

function TdxPDFViewerCustomAction.IsEnabled: Boolean;
begin
  Result := Control.CanFocusEx and Control.IsDocumentLoaded;
end;

procedure TdxPDFViewerCustomAction.SetControl(AValue: TdxPDFViewer);
begin
  inherited SetControl(AValue);
end;

procedure TdxPDFViewerCustomAction.DoExecute;
begin
//do nothing
end;

procedure TdxPDFViewerCustomAction.DoUpdateState;
begin
  Enabled := IsEnabled;
end;

procedure TdxPDFViewerCustomAction.DoResetState;
begin
  Enabled := True;
end;

procedure TdxPDFViewerCustomAction.UpdateControl(Target: TObject);
begin
  if Target is TdxPDFViewer then
    Control := TdxPDFViewer(Target);
end;

function TdxPDFViewerCustomAction.GetControl: TdxPDFViewer;
begin
  Result := TdxPDFViewer(inherited Control);
end;

{ TdxPDFViewerCheckableAction }

constructor TdxPDFViewerCheckableAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
end;

procedure TdxPDFViewerCheckableAction.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := IsChecked;
end;

procedure TdxPDFViewerCheckableAction.DoResetState;
begin
  inherited DoResetState;
  Checked := False;
end;

function TdxPDFViewerCheckableAction.IsChecked: Boolean;
begin
  Result := IsEnabled;
end;

{ TdxPDFViewerOpenDocument }

constructor TdxPDFViewerOpenDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionOpenDocumentCaption;
  FDefaultHintResString := @sdxPDFViewerActionOpenDocumentHint;
  FDefaultImageNameInIconLibrary := 'Actions\Open.png';
end;

function TdxPDFViewerOpenDocument.IsEnabled: Boolean;
begin
  Result := True;
end;

procedure TdxPDFViewerOpenDocument.DoExecute;
var
  AFileName: string;
begin
  if dxPDFViewerOpenDocumentDialog(AFileName) then
  begin
    Control.LoadFromFile(AFileName);
    Control.SetFocus;
  end;
end;

{ TdxPDFViewerCloseDocument }

constructor TdxPDFViewerCloseDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionCloseDocumentCaption;
  FDefaultHintResString := @sdxPDFViewerActionCloseDocumentHint;
  FDefaultImageNameInIconLibrary := 'Actions\Close.png';
end;

procedure TdxPDFViewerCloseDocument.DoExecute;
begin
  Control.Clear;
end;

{ TdxPDFViewerGoToNextPage }

constructor TdxPDFViewerGoToNextPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionGoToNextPageCaption;
  FDefaultHintResString := @sdxPDFViewerActionGoToNextPageHint;
  FDefaultImageNameInIconLibrary := 'Arrows\Next.png';
end;

function TdxPDFViewerGoToNextPage.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (Control.CurrentPageIndex < Control.PageCount - 1);
end;

procedure TdxPDFViewerGoToNextPage.DoExecute;
begin
  Control.GoToNextPage;
end;

{ TdxPDFViewerGoToFirstPage }

constructor TdxPDFViewerGoToFirstPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionGoToFirstPageCaption;
  FDefaultHintResString := @sdxPDFViewerActionGoToFirstPageHint;
  FDefaultImageNameInIconLibrary := 'Arrows\First.png';
end;

function TdxPDFViewerGoToFirstPage.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (Control.CurrentPageIndex <> 0);
end;

procedure TdxPDFViewerGoToFirstPage.DoExecute;
begin
  Control.GoToFirstPage;
end;

{ TdxPDFViewerGoToLastPage }

constructor TdxPDFViewerGoToLastPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionGoToLastPageCaption;
  FDefaultHintResString := @sdxPDFViewerActionGoToLastPageHint;
  FDefaultImageNameInIconLibrary := 'Arrows\Last.png';
end;

function TdxPDFViewerGoToLastPage.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (Control.CurrentPageIndex < Control.PageCount - 1);
end;

procedure TdxPDFViewerGoToLastPage.DoExecute;
begin
  Control.GoToLastPage;
end;

{ TdxPDFViewerGoToPrevPage }

constructor TdxPDFViewerGoToPrevPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionGoToPrevPageCaption;
  FDefaultHintResString := @sdxPDFViewerActionGoToPrevPageHint;
  FDefaultImageNameInIconLibrary := 'Arrows\Prev.png';
end;

function TdxPDFViewerGoToPrevPage.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (Control.CurrentPageIndex > 0);
end;

procedure TdxPDFViewerGoToPrevPage.DoExecute;
begin
  Control.GoToPrevPage;
end;

{ TdxPDFViewerZoomIn }

constructor TdxPDFViewerZoomIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionZoomInCaption;
  FDefaultHintResString := @sdxPDFViewerActionZoomInHint;
  FDefaultImageNameInIconLibrary := 'Zoom\ZoomIn.png';
end;

function TdxPDFViewerZoomIn.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and TdxPDFViewerAccess(Control).CanZoomIn;
end;

procedure TdxPDFViewerZoomIn.DoExecute;
begin
  Control.ZoomIn;
end;

{ TdxPDFViewerZoomOut }

constructor TdxPDFViewerZoomOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionZoomOutCaption;
  FDefaultHintResString := @sdxPDFViewerActionZoomOutHint;
  FDefaultImageNameInIconLibrary := 'Zoom\ZoomOut.png';
end;

function TdxPDFViewerZoomOut.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and TdxPDFViewerAccess(Control).CanZoomOut;
end;

procedure TdxPDFViewerZoomOut.DoExecute;
begin
  Control.ZoomOut;
end;

{ TdxPDFViewerFixedZoomAction }

constructor TdxPDFViewerFixedZoomAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := GetFixedZoomActionCaption;
end;

function TdxPDFViewerFixedZoomAction.IsChecked: Boolean;
begin
  Result := Control.OptionsZoom.ZoomFactor = GetFixedZoomValue;
end;

procedure TdxPDFViewerFixedZoomAction.DoExecute;
begin
  Control.OptionsZoom.ZoomFactor := GetFixedZoomValue;
end;

function TdxPDFViewerFixedZoomAction.GetFixedZoomActionCaption: Pointer;
begin
  Result := nil;
end;

function TdxPDFViewerFixedZoomAction.GetFixedZoomValue: Integer;
begin
  Result := 100;
end;

{ TdxPDFViewerZoom10 }

function TdxPDFViewerZoom10.GetFixedZoomActionCaption: Pointer;
begin
  Result := @sdxPDFViewerActionZoom10Caption;
end;

function TdxPDFViewerZoom10.GetFixedZoomValue: Integer;
begin
  Result := 10;
end;

{ TdxPDFViewerZoom25 }

function TdxPDFViewerZoom25.GetFixedZoomActionCaption: Pointer;
begin
  Result := @sdxPDFViewerActionZoom25Caption;
end;

function TdxPDFViewerZoom25.GetFixedZoomValue: Integer;
begin
  Result := 25;
end;

{ TdxPDFViewerZoom50 }

function TdxPDFViewerZoom50.GetFixedZoomActionCaption: Pointer;
begin
  Result := @sdxPDFViewerActionZoom50Caption;
end;

function TdxPDFViewerZoom50.GetFixedZoomValue: Integer;
begin
  Result := 50;
end;

{ TdxPDFViewerZoom75 }

function TdxPDFViewerZoom75.GetFixedZoomActionCaption: Pointer;
begin
  Result := @sdxPDFViewerActionZoom75Caption;
end;

function TdxPDFViewerZoom75.GetFixedZoomValue: Integer;
begin
  Result := 75;
end;

{ TdxPDFViewerZoom125 }

function TdxPDFViewerZoom125.GetFixedZoomActionCaption: Pointer;
begin
  Result := @sdxPDFViewerActionZoom125Caption;
end;

function TdxPDFViewerZoom125.GetFixedZoomValue: Integer;
begin
  Result := 125;
end;

{ TdxPDFViewerZoom150 }

function TdxPDFViewerZoom150.GetFixedZoomActionCaption: Pointer;
begin
  Result := @sdxPDFViewerActionZoom150Caption;
end;

function TdxPDFViewerZoom150.GetFixedZoomValue: Integer;
begin
  Result := 150;
end;

{ TdxPDFViewerZoom200 }

function TdxPDFViewerZoom200.GetFixedZoomActionCaption: Pointer;
begin
  Result := @sdxPDFViewerActionZoom200Caption;
end;

function TdxPDFViewerZoom200.GetFixedZoomValue: Integer;
begin
  Result := 200;
end;

{ TdxPDFViewerZoom400 }

function TdxPDFViewerZoom400.GetFixedZoomActionCaption: Pointer;
begin
  Result := @sdxPDFViewerActionZoom400Caption;
end;

function TdxPDFViewerZoom400.GetFixedZoomValue: Integer;
begin
  Result := 400;
end;

{ TdxPDFViewerZoom500 }

function TdxPDFViewerZoom500.GetFixedZoomActionCaption: Pointer;
begin
  Result := @sdxPDFViewerActionZoom500Caption;
end;

function TdxPDFViewerZoom500.GetFixedZoomValue: Integer;
begin
  Result := 500;
end;

{ TdxPDFViewerZoom100 }

function TdxPDFViewerZoom100.GetFixedZoomActionCaption: Pointer;
begin
  Result := @sdxPDFViewerActionZoom100Caption;
end;

{ TdxPDFViewerZoomActualSize }

constructor TdxPDFViewerZoomActualSize.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionZoomActualSizeCaption;
  FDefaultImageNameInIconLibrary := 'Zoom\Zoom100.png';
end;

{ TdxPDFViewerZoomToPageLevel }

constructor TdxPDFViewerZoomToPageLevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionZoomToPageLevelCaption;
end;

function TdxPDFViewerZoomToPageLevel.IsChecked: Boolean;
begin
  Result := Control.OptionsZoom.ZoomMode = pzmPages;
end;

procedure TdxPDFViewerZoomToPageLevel.DoExecute;
begin
  Control.OptionsZoom.ZoomMode := pzmPages;
end;

{ TdxPDFViewerZoomFitWidth }

constructor TdxPDFViewerZoomFitWidth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionZoomFitWidthCaption;
end;

function TdxPDFViewerZoomFitWidth.IsChecked: Boolean;
begin
  Result := Control.OptionsZoom.ZoomMode = pzmPageWidth;
end;

procedure TdxPDFViewerZoomFitWidth.DoExecute;
begin
  Control.OptionsZoom.ZoomMode := pzmPageWidth;
end;

{ TdxPDFViewerRotateClockwise }

constructor TdxPDFViewerRotateClockwise.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionRotateClockwiseCaption;
  FDefaultHintResString := @sdxPDFViewerActionRotateClockwiseHint;
  FDefaultImageNameInIconLibrary := 'Actions\Refresh2.png';
end;

procedure TdxPDFViewerRotateClockwise.DoExecute;
begin
  Control.RotateClockwise;
end;

{ TdxPDFViewerRotateCounterclockwise }

constructor TdxPDFViewerRotateCounterclockwise.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionRotateCounterclockwiseCaption;
  FDefaultHintResString := @sdxPDFViewerActionRotateCounterclockwiseHint;
  FDefaultImageNameInIconLibrary := 'Actions\Reset2.png';
end;

procedure TdxPDFViewerRotateCounterclockwise.DoExecute;
begin
  Control.RotateCounterclockwise;
end;

{ TdxPDFViewerGoToNextView }

constructor TdxPDFViewerGoToNextView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionGoToNextViewCaption;
  FDefaultHintResString := @sdxPDFViewerActionGoToNextViewHint;
  FDefaultImageNameInIconLibrary := 'PDF Viewer\NextView.png';
end;

function TdxPDFViewerGoToNextView.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and Control.CanGoToNextView;
end;

procedure TdxPDFViewerGoToNextView.DoExecute;
begin
  Control.GoToNextView;
end;

{ TdxPDFViewerGoToPrevView }

constructor TdxPDFViewerGoToPrevView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionGoToPrevViewCaption;
  FDefaultHintResString := @sdxPDFViewerActionGoToPrevViewHint;
  FDefaultImageNameInIconLibrary := 'PDF Viewer\PreviousView.png';
end;

function TdxPDFViewerGoToPrevView.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and Control.CanGoToPrevView;
end;

procedure TdxPDFViewerGoToPrevView.DoExecute;
begin
  Control.GoToPrevView;
end;

{ TdxPDFViewerHandTool }

constructor TdxPDFViewerSelectTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionSelectToolCaption;
  FDefaultImageNameInIconLibrary := 'PDF Viewer\SelectTool.png';
end;

function TdxPDFViewerSelectTool.IsChecked: Boolean;
begin
  Result := inherited IsChecked and not Control.HandTool;
end;

procedure TdxPDFViewerSelectTool.DoExecute;
begin
  Control.HandTool := IsChecked;
end;

{ TdxPDFViewerHandTool }

constructor TdxPDFViewerHandTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionHandToolCaption;
  FDefaultImageNameInIconLibrary := 'PDF Viewer\HandTool.png';
end;

function TdxPDFViewerHandTool.IsChecked: Boolean;
begin
  Result := inherited IsChecked and Control.HandTool;
end;

procedure TdxPDFViewerHandTool.DoExecute;
begin
  Control.HandTool := not IsChecked;
end;

{ TdxPDFViewerSelectAll }

constructor TdxPDFViewerSelectAll.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionSelectAllCaption;
  FDefaultHintResString := @sdxPDFViewerActionSelectAllHint;
  FDefaultImageNameInIconLibrary := 'Actions\SelectAll.png';
end;

procedure TdxPDFViewerSelectAll.DoExecute;
begin
  Control.Selection.SelectAll;
end;

{ TdxPDFViewerShowPrintForm }

constructor TdxPDFViewerShowPrintForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionPrintCaption;
  FDefaultHintResString := @sdxPDFViewerActionPrintHint;
  FDefaultImageNameInIconLibrary := 'Print\PrintDialog.png';
end;

function TdxPDFViewerShowPrintForm.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and TdxPDFViewerAccess(Control).CanPrint;
end;

function TdxPDFViewerShowPrintForm.GetControlClass: TWinControlClass;
begin
  Result := TdxPDFViewer;
end;

{ TdxPDFViewerFind }

constructor TdxPDFViewerFind.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxPDFViewerActionFindCaption;
  FDefaultHintResString := @sdxPDFViewerActionFindHint;
  FDefaultImageNameInIconLibrary := 'Find\Find.png';
end;

function TdxPDFViewerFind.IsChecked: Boolean;
begin
  Result := inherited IsChecked and Control.IsFindPanelVisible;
end;

function TdxPDFViewerFind.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and TdxPDFViewerAccess(Control).CanChangeVisibility;
end;

procedure TdxPDFViewerFind.DoExecute;
begin
  if Control.IsFindPanelVisible then
    Control.HideFindPanel
  else
    Control.ShowFindPanel;
end;

end.
