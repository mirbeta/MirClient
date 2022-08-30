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

unit dxPDFViewerReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  Classes, Dialogs, DesignEditors, DesignIntf, DesignMenus,
  dxCoreReg, cxClasses, dxPDFViewer, dxBuiltInPopupMenu,  dxUIGeneratorDesignHelpers, dxPDFViewerUIGeneratorScheme;

const
  sdxPDFViewerProductName = 'ExpressPDFViewer';

type
  TdxPDFCustomViewerAccess = class(TdxPDFCustomViewer);

  { TdxPDFViewerComponentEditor }

  TdxPDFViewerComponentEditor = class(TdxUIGeneratorComponentEditor)
  protected
    function GetProductName: string; override;
  end;

  { TdxPDFViewerSelectionEditor }

  TdxPDFViewerSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

{ TdxPDFViewerComponentEditor }

function TdxPDFViewerComponentEditor.GetProductName: string;
begin
  Result := sdxPDFViewerProductName;
end;

{ TdxPDFViewerSelectionEditor }

procedure TdxPDFViewerSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
  Proc('dxPDFCore');
  Proc('dxPDFBase');
  Proc('dxPDFText');
  Proc('dxPDFRecognizedObject');
  Proc('dxPDFDocument');
  if not TdxBuiltInPopupMenuAdapterManager.IsActualAdapterStandard then
    Proc(cxGetUnitName(TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass));
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterComponents(dxCoreLibraryProductPage, [TdxPDFViewer]);
  RegisterComponentEditor(TdxPDFViewer, TdxPDFViewerComponentEditor);
  RegisterSelectionEditor(TdxPDFViewer, TdxPDFViewerSelectionEditor);
end;

end.
