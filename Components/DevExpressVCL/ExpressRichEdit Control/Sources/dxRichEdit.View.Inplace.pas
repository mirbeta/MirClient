{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.View.Inplace;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Windows, Classes, Controls, StdCtrls, Messages,
  dxCore, dxCoreClasses,
  dxRichEdit.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.View.Core,
  dxRichEdit.View.Simple;

type
  { TdxInplaceView }

  TdxInplaceView = class(TdxSimpleView)
  protected
    function CreatePadding: TdxRichEditControlPadding; override;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    function CalculateVerticalScrollbarAutoVisibility: Boolean;
    procedure UpdateVerticalScrollBar; override;
  end;

  { TdxInplaceRichEditViewRepository }

  TdxInplaceRichEditViewRepository = class(TdxRichEditCustomViewRepository)
  protected
    procedure CreateViews; override;
  public
    function GetViewByType(AType: TdxRichEditViewType): TdxRichEditView; override;
  end;

implementation

uses
  dxTypeHelpers, cxGeometry, dxThreading,
  dxRichEdit.Utils.BackgroundThreadUIUpdater;

{ TdxInplaceView }

constructor TdxInplaceView.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
end;

function TdxInplaceView.CalculateVerticalScrollbarAutoVisibility: Boolean;
var
  ASize: TSize;
begin
  ASize := CalcBestSize(WordWrap);
  Result := ASize.cy > Control.ViewBounds.Height;
end;

function TdxInplaceView.CreatePadding: TdxRichEditControlPadding;
begin
  Result := TdxRichEditControlPadding.Create(Self, cxSimpleRect);
end;

procedure TdxInplaceView.UpdateVerticalScrollBar;
var
  APrevVisibility: Boolean;
begin
  APrevVisibility := VerticalScrollController.IsScrollPossible;
  inherited UpdateVerticalScrollBar;
  if APrevVisibility <> VerticalScrollController.IsScrollPossible then
    TdxUIThreadSyncService.EnqueueInvokeInUIThread(Self, ControlResizeDelegate);
end;

{ TdxInplaceRichEditViewRepository }

procedure TdxInplaceRichEditViewRepository.CreateViews;
begin
  AddView(TdxInplaceView.Create(RichEditControl));
end;

function TdxInplaceRichEditViewRepository.GetViewByType(
  AType: TdxRichEditViewType): TdxRichEditView;
begin
  Result := Views[0];
end;

end.
