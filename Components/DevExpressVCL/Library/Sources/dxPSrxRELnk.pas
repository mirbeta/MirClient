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

unit dxPSrxRELnk;

interface

{$I cxVer.inc}

uses
  Classes, Windows, dxPSRELnk, ARichEd;

type
  TrxRichEditReportLink = class(TAbstractdxRichEditReportLink)
  private
    function GetrxRichEdit: TAutoRichEdit;
  protected
    function GetRichEditHandle: HWND; override;
  public
    constructor Create(AOwner: TComponent); override;
    property rxRichEdit: TAutoRichEdit read GetrxRichEdit;
  end;

implementation

uses
  dxPSCore;

constructor TrxRichEditReportLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RichEditVersion := 2;
  LinkModified(False);
end;

function TrxRichEditReportLink.GetrxRichEdit: TAutoRichEdit;
begin
  Result := TAutoRichEdit(Component);
end;

function TrxRichEditReportLink.GetRichEditHandle: HWND;
begin
  if rxRichEdit <> nil then
    Result := rxRichEdit.Handle
  else
    Result := 0;
end;

initialization
  dxPSRegisterReportLink(TrxRichEditReportLink, TAutoRichEdit, nil);

finalization
  dxPSUnregisterReportLink(TrxRichEditReportLink, TAutoRichEdit, nil);

end.

