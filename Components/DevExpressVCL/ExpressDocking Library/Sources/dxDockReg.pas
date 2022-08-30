{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDocking                                           }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDOCKING AND ALL ACCOMPANYING   }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxDockReg;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Forms, SysUtils, Graphics, Controls, ImgList,
  DesignIntf, ComponentDesigner, DesignEditors, VCLEditors,
  dxCoreReg, cxLibraryReg, cxPropEditors, cxControls, dxCore;

const
  dxDockProductName = 'ExpressDocking Library';
  dxDockProductPage = 'ExpressDocking';

type

  { TdxDockingComponentEditor }

  TdxDockingComponentEditor = class(TdxComponentEditor)
  protected
    function GetProductName: string; override;
  end;

  { TdxDockingImageIndexProperty }

  TdxDockingImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TdxDockingCaptionButtonImageIndexProperty }

  TdxDockingCaptionButtonImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

procedure Register;

implementation

uses
  dxDockControl, dxDockPanel, TypInfo, cxDesignWindows, dxMessages, cxGraphics;

type
  TdxCustomDockControlAccess = class(TdxCustomDockControl);

  { TdxDockDesignWindow }

  TdxDockDesignWindow = class(TcxGlobalDesignWindow)
  public
    procedure SelectionsChanged(const ASelection: TDesignerSelectionList); override;
  end;

  { TdxDockingSelectionEditor }

  TdxDockingSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

var
  FdxDockDesignWindow: TdxDockDesignWindow;

{ TdxDockingSelectionEditor }

procedure TdxDockingSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
  dxSkinsRequiresAdditionalUnits(TdxDockingManager, Proc);
  Proc('cxPC');
end;

{ TdxDockingComponentEditor }

function TdxDockingComponentEditor.GetProductName: string;
begin
  Result := dxDockProductName;
end;

{ TdxDockingImageIndexProperty  }

function TdxDockingImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TdxCustomDockSite(GetComponent(0)).Images;
end;

{ TdxDockDesignWindow }

procedure TdxDockDesignWindow.SelectionsChanged(const ASelection: TDesignerSelectionList);
var
  I: Integer;
begin
  for I := 0 to ASelection.Count - 1 do
  begin
    if (ASelection[I] is TdxCustomDockControl) and (TdxCustomDockControl(ASelection[I]).ParentDockControl = nil) then
      (ASelection[I] as TdxCustomDockControl).Repaint;
  end;
end;

{ TdxDockingCaptionButtonImageIndexProperty }

function TdxDockingCaptionButtonImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TdxDockControlCustomCaptionButtons(TdxDockControlCaptionButton(GetComponent(0)).Collection.Owner).Images;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents(dxDockProductPage, [TdxDockingManager, TdxDockPanel, TdxDockSite]);
  RegisterPropertyEditor(TypeInfo(Integer), TdxCustomDockControl, 'ImageIndex', TdxDockingImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxDockControlCaptionButton,
    'ImageIndex', TdxDockingCaptionButtonImageIndexProperty);
  RegisterComponentEditor(TdxCustomDockControl, TdxDockingComponentEditor);
  RegisterSelectionEditor(TdxCustomDockControl, TdxDockingSelectionEditor);
  RegisterComponentEditor(TdxDockingManager, TdxDockingComponentEditor);
end;

procedure RegisterDockControl(ASender: TObject);
begin
  if dxDockingController.DockControlCount = 1 then
    FdxDockDesignWindow := TdxDockDesignWindow.Create(nil);
end;

procedure UnregisterDockControl(ASender: TObject);
begin
  if (FdxDockDesignWindow <> nil) and ((dxDockingController = nil) or (dxDockingController.DockControlCount = 1)) then
    cxReleaseForm(FdxDockDesignWindow);
end;

initialization
  FOnRegisterDockControl := RegisterDockControl;
  FOnUnregisterDockControl := UnregisterDockControl;

end.
