{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSPrVwDsg;

interface

{$I cxVer.inc}

uses
  DesignIntf, dxPSDsgProxies, dxPSCore;

procedure dxShowPreviewWindow(AComponentPrinter: TCustomdxComponentPrinter; AFormDesigner: TFormDesigner);

implementation

type
  TdxPreviewWindowDesigner = class(TAbstractdxPreviewWindowDesigner)
  private
    FFormDesigner: TFormDesigner;
  protected
    procedure Activate; override;
    procedure Modified; override;
  public
    constructor Create(AComponentPrinter: TCustomdxComponentPrinter; AFormDesigner: TFormDesigner);
    destructor Destroy; override;
  end;

procedure dxShowPreviewWindow(AComponentPrinter: TCustomdxComponentPrinter;
  AFormDesigner: TFormDesigner);
begin
  if AComponentPrinter.PreviewWindowDesigner = nil then
    TdxPreviewWindowDesigner.Create(AComponentPrinter, AFormDesigner);
  TdxPreviewWindowDesigner(AComponentPrinter.PreviewWindowDesigner).Activate;
end;

{ TdxPreviewWindowDesigner }

constructor TdxPreviewWindowDesigner.Create(AComponentPrinter: TCustomdxComponentPrinter;
  AFormDesigner: TFormDesigner);
begin
  inherited Create(AComponentPrinter);
  FFormDesigner := AFormDesigner;
end;

destructor TdxPreviewWindowDesigner.Destroy;
begin
  FFormDesigner := nil;
  inherited;
end;

procedure TdxPreviewWindowDesigner.Activate;
begin
  ComponentPrinter.Preview(True, nil);
  Modified;
end;

procedure TdxPreviewWindowDesigner.Modified;
begin
  if FFormDesigner <> nil then FFormDesigner.Modified;
end;

end.

