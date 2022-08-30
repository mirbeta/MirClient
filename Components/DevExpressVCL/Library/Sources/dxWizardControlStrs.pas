{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressWizardControl                                     }
{                                                                    }
{           Copyright (c) 2012-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSWIZARDCONTROL AND ALL          }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxWizardControlStrs;

{$I cxVer.inc}

interface

resourcestring
  sdxWizardControlErrorWrongChild = 'You can only insert a TdxWizardControlPage into TdxWizardControl.';
  sdxWizardControlErrorWrongParent = 'You can insert a TdxWizardControlPage only into TdxWizardControl.';
  sdxWizardControlErrorWrongPageIndex = '%d is an invalid PageIndex value. PageIndex must be between 0 and %d';

  // Buttons
  sdxWizardControlButtonBack = '&Back';
  sdxWizardControlButtonCancel = '&Cancel';
  sdxWizardControlButtonFinish = '&Finish';
  sdxWizardControlButtonHelp = '&Help';
  sdxWizardControlButtonNext = '&Next';

  sdxWizardControlPageDefaultTitle = 'Page Title';
  sdxWizardControlPageDefaultDescription = 'Page description: this should help the user complete a subtask';

implementation

uses
  dxCore;

procedure AddWizardControlResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxWizardControlErrorWrongChild', @sdxWizardControlErrorWrongChild);
  AProduct.Add('sdxWizardControlErrorWrongParent', @sdxWizardControlErrorWrongParent);
  AProduct.Add('sdxWizardControlErrorWrongPageIndex', @sdxWizardControlErrorWrongPageIndex);

  AProduct.Add('sdxWizardControlButtonBack', @sdxWizardControlButtonBack);
  AProduct.Add('sdxWizardControlButtonCancel', @sdxWizardControlButtonCancel);
  AProduct.Add('sdxWizardControlButtonFinish', @sdxWizardControlButtonFinish);
  AProduct.Add('sdxWizardControlButtonHelp', @sdxWizardControlButtonHelp);
  AProduct.Add('sdxWizardControlButtonNext', @sdxWizardControlButtonNext);

  AProduct.Add('sdxWizardControlPageDefaultTitle', @sdxWizardControlPageDefaultTitle);
  AProduct.Add('sdxWizardControlPageDefaultDescription', @sdxWizardControlPageDefaultDescription);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressWizardControl', @AddWizardControlResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressWizardControl');

end.

