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

unit dxRichEdit.Strs;

{$I cxVer.inc}

interface

uses
  dxCore, cxClasses;

const
  dxRichEditProductName = 'ExpressRichEditControl';

resourcestring
  sdxRichEditFileFilterDescription_AllFiles = 'All Files';
  sdxRichEditFileFilterDescription_RtfFiles = 'Rich Text Format';
  sdxRichEditFileFilterDescription_OpenXmlFiles = 'Word 2007 Document';
  sdxRichEditFileFilterDescription_TextFiles = 'Text Files';
  sdxRichEditFileFilterDescription_HtmlFiles = 'HyperText Markup Language Format';
  sdxRichEditFileFilterDescription_DocFiles = 'Microsoft Word Document';

  sdxRichEditFileFilterDescription_BitmapFiles = 'Windows Bitmap';
  sdxRichEditFileFilterDescription_JPEGFiles = 'JPEG File Interchange Format';
  sdxRichEditFileFilterDescription_PNGFiles = 'Portable Network Graphics';
  sdxRichEditFileFilterDescription_GifFiles = 'Graphics Interchange Format';
  sdxRichEditFileFilterDescription_TiffFiles = 'Tag Image File Format';
  sdxRichEditFileFilterDescription_EmfFiles = 'Microsoft Enhanced Metafile';
  sdxRichEditFileFilterDescription_WmfFiles = 'Windows Metafile';

  sdxCurrentDocumentHyperlinkTooltip = 'Current Document';

  sdxRichEditCaption_PageHeader = 'Header';
  sdxRichEditCaption_FirstPageHeader = 'First Page Header';
  sdxRichEditCaption_OddPageHeader = 'Odd Page Header';
  sdxRichEditCaption_EvenPageHeader = 'Even Page Header';
  sdxRichEditCaption_PageFooter = 'Footer';
  sdxRichEditCaption_FirstPageFooter = 'First Page Footer';
  sdxRichEditCaption_OddPageFooter = 'Odd Page Footer';
  sdxRichEditCaption_EvenPageFooter = 'Even Page Footer';
  sdxRichEditCaption_SameAsPrevious = 'Same as Previous';

  sdxRichEditMsg_Loading = 'Loading...';
  sdxRichEditMsg_Saving  = 'Saving...';
  sdxRichEditMsg_EncryptedFile = 'Encrypted files are not currently supported';

  sdxRichEditCaptionUnitPercent = 'Percent';
  sdxRichEditCaptionUnitInches = 'Inches';
  sdxRichEditCaptionUnitCentimeters = 'Centimeters';
  sdxRichEditCaptionUnitMillimeters = 'Millimeters';
  sdxRichEditCaptionUnitPoints = 'Points';
  sdxRichEditUnitsInches = '"';
  sdxRichEditUnitsCentimeters = ' cm';
  sdxRichEditUnitsMillimeters = ' mm';
  sdxRichEditUnitsPoints = ' pt';
  sdxRichEditUnitsPicas = ' pi';
  sdxRichEditUnitsPercent = '%';

  sdxRichEditConfirmSaveDocumentWithPasswordProtection = 'This document is password protected. Saving it in a format ' +
    'other than a Word document will result in the loss of this protection. Do you want to continue?';

procedure AddRichEditResourceStringNames(AProduct: TdxProductResourceStrings);

implementation

procedure AddRichEditResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxRichEditFileFilterDescription_AllFiles', @sdxRichEditFileFilterDescription_AllFiles);
  AProduct.Add('sdxRichEditFileFilterDescription_RtfFiles', @sdxRichEditFileFilterDescription_RtfFiles);
  AProduct.Add('sdxRichEditFileFilterDescription_OpenXmlFiles', @sdxRichEditFileFilterDescription_OpenXmlFiles);
  AProduct.Add('sdxRichEditFileFilterDescription_TextFiles', @sdxRichEditFileFilterDescription_TextFiles);
  AProduct.Add('sdxRichEditFileFilterDescription_HtmlFiles', @sdxRichEditFileFilterDescription_HtmlFiles);
  AProduct.Add('sdxRichEditFileFilterDescription_DocFiles', @sdxRichEditFileFilterDescription_DocFiles);

  AProduct.Add('sdxRichEditFileFilterDescription_BitmapFiles', @sdxRichEditFileFilterDescription_BitmapFiles);
  AProduct.Add('sdxRichEditFileFilterDescription_JPEGFiles', @sdxRichEditFileFilterDescription_JPEGFiles);
  AProduct.Add('sdxRichEditFileFilterDescription_PNGFiles', @sdxRichEditFileFilterDescription_PNGFiles);
  AProduct.Add('sdxRichEditFileFilterDescription_GifFiles', @sdxRichEditFileFilterDescription_GifFiles);
  AProduct.Add('sdxRichEditFileFilterDescription_TiffFiles', @sdxRichEditFileFilterDescription_TiffFiles);
  AProduct.Add('sdxRichEditFileFilterDescription_EmfFiles', @sdxRichEditFileFilterDescription_EmfFiles);
  AProduct.Add('sdxRichEditFileFilterDescription_WmfFiles', @sdxRichEditFileFilterDescription_WmfFiles);
  AProduct.Add('sdxCurrentDocumentHyperlinkTooltip', @sdxCurrentDocumentHyperlinkTooltip);

  AProduct.Add('sdxRichEditCaption_PageHeader', @sdxRichEditCaption_PageHeader);
  AProduct.Add('sdxRichEditCaption_FirstPageHeader', @sdxRichEditCaption_FirstPageHeader);
  AProduct.Add('sdxRichEditCaption_OddPageHeader', @sdxRichEditCaption_OddPageHeader);
  AProduct.Add('sdxRichEditCaption_EvenPageHeader', @sdxRichEditCaption_EvenPageHeader);
  AProduct.Add('sdxRichEditCaption_PageFooter', @sdxRichEditCaption_PageFooter);
  AProduct.Add('sdxRichEditCaption_FirstPageFooter', @sdxRichEditCaption_FirstPageFooter);
  AProduct.Add('sdxRichEditCaption_OddPageFooter', @sdxRichEditCaption_OddPageFooter);
  AProduct.Add('sdxRichEditCaption_EvenPageFooter', @sdxRichEditCaption_EvenPageFooter);
  AProduct.Add('sdxRichEditCaption_SameAsPrevious', @sdxRichEditCaption_SameAsPrevious);

  AProduct.Add('sdxRichEditMsg_Loading', @sdxRichEditMsg_Loading);
  AProduct.Add('sdxRichEditMsg_Saving', @sdxRichEditMsg_Saving);
  AProduct.Add('sdxRichEditMsg_EncryptedFile', @sdxRichEditMsg_EncryptedFile);

  AProduct.Add('sdxRichEditCaptionUnitPercent', @sdxRichEditCaptionUnitPercent);
  AProduct.Add('sdxRichEditCaptionUnitInches', @sdxRichEditCaptionUnitInches);
  AProduct.Add('sdxRichEditCaptionUnitCentimeters', @sdxRichEditCaptionUnitCentimeters);
  AProduct.Add('sdxRichEditCaptionUnitMillimeters', @sdxRichEditCaptionUnitMillimeters);
  AProduct.Add('sdxRichEditCaptionUnitPoints', @sdxRichEditCaptionUnitPoints);
  AProduct.Add('sdxRichEditUnitsInches', @sdxRichEditUnitsInches);
  AProduct.Add('sdxRichEditUnitsCentimeters', @sdxRichEditUnitsCentimeters);
  AProduct.Add('sdxRichEditUnitsMillimeters', @sdxRichEditUnitsMillimeters);
  AProduct.Add('sdxRichEditUnitsPoints', @sdxRichEditUnitsPoints);
  AProduct.Add('sdxRichEditUnitsPicas', @sdxRichEditUnitsPicas);
  AProduct.Add('sdxRichEditUnitsPercent', @sdxRichEditUnitsPercent);

  AProduct.Add('sdxRichEditConfirmSaveDocumentWithPasswordProtection', @sdxRichEditConfirmSaveDocumentWithPasswordProtection);
end;

initialization
  dxResourceStringsRepository.RegisterProduct(dxRichEditProductName, @AddRichEditResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct(dxRichEditProductName, @AddRichEditResourceStringNames);

end.
