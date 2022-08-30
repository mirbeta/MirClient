{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit cxExtEditConsts;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Classes, Graphics, SysUtils, cxContainer;

resourcestring
  scxSEditRepositoryColorComboBoxItem = 'ColorComboBox|Represents a combo box to allow selection of a color';
  scxSEditRepositoryColorEditItem = 'ColorEdit|Represents an editor with a dropdown color gallery';
  scxSEditRepositoryFontNameComboBoxItem = 'FontNameComboBox|Represents a combo box to allow selection of a font';
  scxSEditRepositoryLabelItem = 'Label|Represents a label';
  scxSEditRepositoryProgressBarItem = 'ProgressBar|Represents an advanced progress bar control';
  scxSEditRepositoryTrackBarItem = 'TrackBar|Represents an editor that provides a slider to select a value on its bar';
  scxSEditRepositoryRangeTrackBarItem = 'RangeTrackBar|Represents an editor that provides two sliders to select a range of values on its bar';
  scxSEditRepositorySpinButtonItem = 'SpinButton|Represents a spin button';
  scxSEditRepositoryCheckComboBox = 'CheckComboBox|Represents a combo box with a dropdown check list';
  scxSEditRepositoryShellComboBoxItem = 'ShellComboBox|Represents a combo box with a dropdown shell tree view';
  scxSEditRepositoryCheckGroupItem = 'CheckGroup|Represents an editor that provides a set of check boxes';
  scxSEditRepositoryRatingControlItem = 'RatingControl|Represents a rating control';
  scxUDAssociated = ' is already associated with ';
  scxHotZoneStyleMediaPlayer9 = 'MediaPlayer9';
  scxHotZoneStyleMediaPlayer8 = 'MediaPlayer8';
  scxHotZoneStyleXPTaskBar = 'XPTaskBar';
  scxHotZoneStyleSimple = 'Simple';
  scxLoadingFonts = 'Loading ...';

  cxSEditCheckGroupCaptionStateDelimiter = ': ';
  cxSEditCheckGroupFilterColumnSeparator = '; ';
  cxSEditCheckGroupChecked = 'Checked';
  cxSEditCheckGroupGrayed = 'Grayed';
  cxSEditCheckGroupUnchecked = 'Unchecked';

  cxSCheckComboBoxStatesItemsPropertyDlgCaption = 'cxCheckComboBox - CheckStates editor';
  cxSCheckGroupStatesItemsPropertyDlgCaption = 'cxCheckGroup - CheckStates editor';
  cxSCheckComboBoxEmptySelectionText = 'None selected';
  cxSCheckControlIncorrectItemCount = 'The number of items cannot be greater than 64, if the EditValueFormat is cvfInteger';

  cxSColorComboBoxDefaultDescription = 'Color not selected';

  cxSEditRichEditLibraryError = 'Cannot load a RichEdit library';
  cxSEditRichEditLineInsertionError = 'RichEdit line insertion error';
  cxSEditRichEditLoadFail = 'Failed to Load Stream';
  cxSEditRichEditSaveFail = 'Failed to Save Stream';
  cxSEditRichEditSelectionSaveFail = 'Failed to selection Save Stream';
  cxSEditRichEditOleInterfaceFail = 'RichEdit: failed to get IRichEditOle interface';
  cxSEditRichEditCallBackFail = 'RichEdit: failed to set callback';
  cxSEditRichEditLinkFail = 'RichEdit: cannot link to an invalid source';
  scxSEditRepositoryRichEditItem = 'RichEdit|Represents an editor that provides rich text formatting capabilities';

  cxSEditRichEditUndoCaption                 = '&Undo';
  cxSEditRichEditRedoCaption                 = '&Redo';
  cxSEditRichEditCutCaption                  = 'Cu&t';
  cxSEditRichEditCopyCaption                 = '&Copy';
  cxSEditRichEditPasteCaption                = '&Paste';
  cxSEditRichEditDeleteCaption               = '&Delete';
  cxSEditRichEditSelectAllCaption            = 'Select &All';

type
  TcxNaturalNumber = 1..High(Integer);
  TcxPositiveNumber = 0..High(Integer);

const
  cxDelphiColorValues: array[0..51] of TColor =
    (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
     clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite,
     clMoneyGreen, clSkyBlue, clCream, clMedGray,
     clActiveBorder, clActiveCaption, clAppWorkSpace, clBackground,
     clBtnFace, clBtnHighlight, clBtnShadow, clBtnText, clCaptionText,
     clDefault,
     clGradientActiveCaption, clGradientInactiveCaption,
     clGrayText, clHighlight, clHighlightText,
     clHotLight,
     clInactiveBorder,
     clInactiveCaption, clInactiveCaptionText, clInfoBk, clInfoText,
     clMenu,
     clMenuBar, clMenuHighlight,
     clMenuText, clNone, clScrollBar,
     cl3DDkShadow, cl3DLight, clWindow, clWindowFrame, clWindowText);

  cxDelphiColorNames:
    array[0..51] of string =
    ('clBlack', 'clMaroon', 'clGreen', 'clOlive', 'clNavy', 'clPurple', 'clTeal',
     'clGray', 'clSilver', 'clRed', 'clLime', 'clYellow', 'clBlue', 'clFuchsia',
     'clAqua', 'clWhite',
     'clMoneyGreen', 'clSkyBlue', 'clCream', 'clMedGray',
     'clActiveBorder', 'clActiveCaption', 'clAppWorkSpace', 'clBackground',
     'clBtnFace', 'clBtnHighlight', 'clBtnShadow', 'clBtnText', 'clCaptionText',
     'clDefault',
     'clGradientActiveCaption', 'clGradientInactiveCaption',
     'clGrayText', 'clHighlight', 'clHighlightText',
     'clHotLight',
     'clInactiveBorder',
     'clInactiveCaption', 'clInactiveCaptionText', 'clInfoBk', 'clInfoText',
     'clMenu',
     'clMenuBar', 'clMenuHighlight',
     'clMenuText', 'clNone', 'clScrollBar',
     'cl3DDkShadow', 'cl3DLight', 'clWindow', 'clWindowFrame', 'clWindowText');

  cxHTML4ColorValues: array[0..15] of TColor =
    ($000000, $C0C0C0, $808080, $FFFFFF, $000080, $0000FF, $800080, $FF00FF,
     $008000, $00FF00, $008080, $00FFFF, $800000, $FF0000, $808000, $FFFF00);

  cxHTML4ColorNames: array[0..15] of string =
    ('Black', 'Silver', 'Gray', 'White', 'Maroon', 'Red', 'Purple', 'Fuchsia',
     'Green', 'Lime', 'Olive', 'Yellow', 'Navy', 'Blue', 'Teal', 'Aqua');

  cxX11ColorValues: array[0..139] of TColor =
    ($FFF8F0, $D7EBFA, $FFFF00, $D4FF7F, $FFFFF0, $DCF5F5, $C4E4FF, $000000,
     $CDEBFF, $FF0000, $E22B8A, $2A2AA5, $87B8DE, $A09E5F, $00FF7F, $1E69D2,
     $507FFF, $ED9564, $DCF8FF, $3C14DC, $FFFF00, $8B0000, $8B8B00, $0B86B8,
     $A9A9A9, $006400, $6BB7BD, $8B008B, $2F6B55, $008CFF, $CC3299, $00008B,
     $7A96E9, $8FBC8F, $8B3D48, $4F4F2F, $D1CE00, $D30094, $9314FF, $FFBF00,
     $696969, $FF901E, $2222B2, $F0FAFF, $228B22, $FF00FF, $DCDCDC, $FFF8F8,
     $00D7FF, $20A5DA, $808080, $008000, $2FFFAD, $F0FFF0, $B469FF, $5C5CCD,
     $82004B, $F0FFFF, $8CE6F0, $FAE6E6, $F5F0FF, $00FC7C, $CDFAFF, $E6D8AD,
     $8080F0, $FFFFE0, $D2FAFA, $90EE90, $D3D3D3, $C1B6FF, $7AA0FF, $AAB220,
     $FACE87, $998877, $DEC4B0, $E0FFFF, $00FF00, $32CD32, $E6F0FA, $FF00FF,
     $000080, $AACD66, $CD0000, $D355BA, $DB7093, $71B33C, $EE687B, $9AFA00,
     $CCD148, $8515C7, $701919, $FAFFF5, $E1E4FF, $B5E4FF, $ADDEFF, $800000,
     $E6F5FD, $008080, $238E6B, $00A5FF, $0045FF, $D670DA, $AAE8EE, $98FB98,
     $EEEEAF, $9370DB, $D5EFFF, $B9DAFF, $3F85CD, $CBC0FF, $DDA0DD, $E6E0B0,
     $800080, $0000FF, $8F8FBC, $E16941, $13458B, $7280FA, $60A4F4, $578B2E,
     $EEF5FF, $2D52A0, $C0C0C0, $EBCE87, $CD5A6A, $908070, $FAFAFF, $7FFF00,
     $B48246, $8CB4D2, $808000, $D8BFD8, $4763FF, $D0E040, $EE82EE, $B3DEF5,
     $FFFFFF, $F5F5F5, $00FFFF, $32CD9A);

  cxX11ColorNames: array[0..139] of string =
    ('AliceBlue', 'AntiqueWhite', 'Aqua', 'Aquamarine', 'Azure', 'Beige',
     'Bisque', 'Black', 'BlanchedAlmond', 'Blue', 'BlueViolet', 'Brown',
     'BurlyWood', 'CadetBlue', 'Chartreuse', 'Chocolate', 'Coral',
     'CornflowerBlue', 'Cornsilk', 'Crimson', 'Cyan', 'DarkBlue', 'DarkCyan',
     'DarkGoldenrod', 'DarkGray', 'DarkGreen', 'DarkKhaki', 'DarkMagenta',
     'DarkOliveGreen', 'DarkOrange', 'DarkOrchid', 'DarkRed', 'DarkSalmon',
     'DarkSeaGreen', 'DarkSlateBlue', 'DarkSlateGray', 'DarkTurquoise',
     'DarkViolet', 'DeepPink', 'DeepSkyBlue', 'DimGray', 'DodgerBlue',
     'FireBrick', 'FloralWhite', 'ForestGreen', 'Fuchsia', 'Gainsboro',
     'GhostWhite', 'Gold', 'Goldenrod', 'Gray', 'Green', 'GreenYellow',
     'Honeydew', 'HotPink', 'IndianRed', 'Indigo', 'Ivory', 'Khaki',
     'Lavender', 'LavenderBlush', 'LawnGreen', 'LemonChiffon', 'LightBlue',
     'LightCoral', 'LightCyan', 'LightGoldenrodYellow', 'LightGreen',
     'LightGrey', 'LightPink', 'LightSalmon', 'LightSeaGreen', 'LightSkyBlue',
     'LightSlateGray', 'LightSteelBlue', 'LightYellow', 'Lime', 'LimeGreen',
     'Linen', 'Magenta', 'Maroon', 'MediumAquamarine', 'MediumBlue',
     'MediumOrchid', 'MediumPurple', 'MediumSeaGreen', 'MediumSlateBlue',
     'MediumSpringGreen', 'MediumTurquoise', 'MediumVioletRed', 'MidnightBlue',
     'MintCream', 'MistyRose', 'Moccasin', 'NavajoWhite', 'Navy', 'OldLace',
     'Olive', 'OliveDrab', 'Orange', 'OrangeRed', 'Orchid', 'PaleGoldenrod',
     'PaleGreen', 'PaleTurquoise', 'PaleVioletRed', 'PapayaWhip', 'PeachPuff',
     'Peru', 'Pink', 'Plum', 'PowderBlue', 'Purple', 'Red', 'RosyBrown',
     'RoyalBlue', 'SaddleBrown', 'Salmon', 'SandyBrown', 'SeaGreen', 'Seashell',
     'Sienna', 'Silver', 'SkyBlue', 'SlateBlue', 'SlateGray', 'Snow',
     'SpringGreen', 'SteelBlue', 'Tan', 'Teal', 'Thistle', 'Tomato', 'Turquoise',
     'Violet', 'Wheat', 'White', 'WhiteSmoke', 'Yellow', 'YellowGreen');

  cxX11OrderedColorValues: array[0..139] of TColor =
    ($C1B6FF, $CBC0FF, $3C14DC, $F5F0FF, $9370DB, $B469FF, $9314FF, $8515C7,
     $D670DA, $D8BFD8, $DDA0DD, $EE82EE, $FF00FF, $FF00FF, $8B008B, $800080,
     $D355BA, $D30094, $CC3299, $82004B, $E22B8A, $DB7093, $EE687B, $CD5A6A,
     $8B3D48, $FAE6E6, $FFF8F8, $FF0000, $CD0000, $701919, $8B0000, $800000,
     $E16941, $ED9564, $DEC4B0, $998877, $908070, $FF901E, $FFF8F0, $B48246,
     $FACE87, $EBCE87, $FFBF00, $E6D8AD, $E6E0B0, $A09E5F, $FFFFF0, $FFFFE0,
     $EEEEAF, $FFFF00, $FFFF00, $D1CE00, $4F4F2F, $8B8B00, $808000, $CCD148,
     $AAB220, $D0E040, $D4FF7F, $AACD66, $9AFA00, $FAFFF5, $7FFF00, $71B33C,
     $578B2E, $F0FFF0, $90EE90, $98FB98, $8FBC8F, $32CD32, $00FF00, $228B22,
     $008000, $006400, $00FF7F, $00FC7C, $2FFFAD, $2F6B55, $32CD9A, $238E6B,
     $DCF5F5, $D2FAFA, $F0FFFF, $E0FFFF, $00FFFF, $008080, $6BB7BD, $CDFAFF,
     $AAE8EE, $8CE6F0, $00D7FF, $DCF8FF, $20A5DA, $0B86B8, $F0FAFF, $E6F5FD,
     $B3DEF5, $B5E4FF, $00A5FF, $D5EFFF, $CDEBFF, $ADDEFF, $D7EBFA, $8CB4D2,
     $87B8DE, $C4E4FF, $008CFF, $E6F0FA, $3F85CD, $B9DAFF, $60A4F4, $1E69D2,
     $13458B, $EEF5FF, $2D52A0, $7AA0FF, $507FFF, $0045FF, $7A96E9, $4763FF,
     $E1E4FF, $7280FA, $FAFAFF, $8080F0, $8F8FBC, $5C5CCD, $0000FF, $2A2AA5,
     $2222B2, $00008B, $000080, $FFFFFF, $F5F5F5, $DCDCDC, $D3D3D3, $C0C0C0,
     $A9A9A9, $808080, $696969, $000000);

  cxX11OrderedColorNames: array[0..139] of string =
    ('LightPink', 'Pink', 'Crimson', 'LavenderBlush', 'PaleVioletRed', 'HotPink',
     'DeepPink', 'MediumVioletRed', 'Orchid', 'Thistle', 'Plum', 'Violet',
     'Magenta', 'Fuchsia', 'DarkMagenta', 'Purple', 'MediumOrchid', 'DarkViolet',
     'DarkOrchid', 'Indigo', 'BlueViolet', 'MediumPurple', 'MediumSlateBlue',
     'SlateBlue', 'DarkSlateBlue', 'Lavender', 'GhostWhite', 'Blue', 'MediumBlue',
     'MidnightBlue', 'DarkBlue', 'Navy', 'RoyalBlue', 'CornflowerBlue',
     'LightSteelBlue', 'LightSlateGray', 'SlateGray', 'DodgerBlue', 'AliceBlue',
     'SteelBlue', 'LightSkyBlue', 'SkyBlue', 'DeepSkyBlue', 'LightBlue',
     'PowderBlue', 'CadetBlue', 'Azure', 'LightCyan', 'PaleTurquoise', 'Cyan',
     'Aqua', 'DarkTurquoise', 'DarkSlateGray', 'DarkCyan', 'Teal',
     'MediumTurquoise', 'LightSeaGreen', 'Turquoise', 'Aquamarine',
     'MediumAquamarine', 'MediumSpringGreen', 'MintCream', 'SpringGreen',
     'MediumSeaGreen', 'SeaGreen', 'Honeydew', 'LightGreen', 'PaleGreen',
     'DarkSeaGreen', 'LimeGreen', 'Lime', 'ForestGreen', 'Green', 'DarkGreen',
     'Chartreuse', 'LawnGreen', 'GreenYellow', 'DarkOliveGreen', 'YellowGreen',
     'OliveDrab', 'Beige', 'LightGoldenrodYellow', 'Ivory', 'LightYellow',
     'Yellow', 'Olive', 'DarkKhaki', 'LemonChiffon', 'PaleGoldenrod', 'Khaki',
     'Gold', 'Cornsilk', 'Goldenrod', 'DarkGoldenrod', 'FloralWhite', 'OldLace',
     'Wheat', 'Moccasin', 'Orange', 'PapayaWhip', 'BlanchedAlmond', 'NavajoWhite',
     'AntiqueWhite', 'Tan', 'BurlyWood', 'Bisque', 'DarkOrange', 'Linen', 'Peru',
     'PeachPuff', 'SandyBrown', 'Chocolate', 'SaddleBrown', 'Seashell', 'Sienna',
     'LightSalmon', 'Coral', 'OrangeRed', 'DarkSalmon', 'Tomato', 'MistyRose',
     'Salmon', 'Snow', 'LightCoral', 'RosyBrown', 'IndianRed', 'Red', 'Brown',
     'FireBrick', 'DarkRed', 'Maroon', 'White', 'WhiteSmoke', 'Gainsboro',
     'LightGrey', 'Silver', 'DarkGray', 'Gray', 'DimGray', 'Black');

implementation

uses
  dxCore;

procedure AddcxExtEditResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAdress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAdress);
  end;

begin
  InternalAdd('scxSEditRepositoryColorComboBoxItem', @scxSEditRepositoryColorComboBoxItem);
  InternalAdd('scxSEditRepositoryColorEditItem', @scxSEditRepositoryColorEditItem);
  InternalAdd('scxSEditRepositoryFontNameComboBoxItem', @scxSEditRepositoryFontNameComboBoxItem);
  InternalAdd('scxSEditRepositoryLabelItem', @scxSEditRepositoryLabelItem);
  InternalAdd('scxSEditRepositoryProgressBarItem', @scxSEditRepositoryProgressBarItem);
  InternalAdd('scxSEditRepositoryTrackBarItem', @scxSEditRepositoryTrackBarItem);
  InternalAdd('scxSEditRepositoryRangeTrackBarItem', @scxSEditRepositoryRangeTrackBarItem);
  InternalAdd('scxSEditRepositorySpinButtonItem', @scxSEditRepositorySpinButtonItem);
  InternalAdd('scxSEditRepositoryCheckComboBox', @scxSEditRepositoryCheckComboBox);
  InternalAdd('scxSEditRepositoryShellComboBoxItem', @scxSEditRepositoryShellComboBoxItem);
  InternalAdd('scxSEditRepositoryCheckGroupItem', @scxSEditRepositoryCheckGroupItem);
  InternalAdd('scxSEditRepositoryRatingControlItem', @scxSEditRepositoryRatingControlItem);
  InternalAdd('scxUDAssociated', @scxUDAssociated);
  InternalAdd('scxHotZoneStyleMediaPlayer9', @scxHotZoneStyleMediaPlayer9);
  InternalAdd('scxHotZoneStyleMediaPlayer8', @scxHotZoneStyleMediaPlayer8);
  InternalAdd('scxHotZoneStyleXPTaskBar', @scxHotZoneStyleXPTaskBar);
  InternalAdd('scxHotZoneStyleSimple', @scxHotZoneStyleSimple);
  InternalAdd('scxLoadingFonts', @scxLoadingFonts);
  InternalAdd('cxSEditCheckGroupCaptionStateDelimiter', @cxSEditCheckGroupCaptionStateDelimiter);
  InternalAdd('cxSEditCheckGroupFilterColumnSeparator', @cxSEditCheckGroupFilterColumnSeparator);
  InternalAdd('cxSEditCheckGroupChecked', @cxSEditCheckGroupChecked);
  InternalAdd('cxSEditCheckGroupGrayed', @cxSEditCheckGroupGrayed);
  InternalAdd('cxSEditCheckGroupUnchecked', @cxSEditCheckGroupUnchecked);
  InternalAdd('cxSCheckComboBoxStatesItemsPropertyDlgCaption', @cxSCheckComboBoxStatesItemsPropertyDlgCaption);
  InternalAdd('cxSCheckControlIncorrectItemCount', @cxSCheckControlIncorrectItemCount);
  InternalAdd('cxSCheckGroupStatesItemsPropertyDlgCaption', @cxSCheckGroupStatesItemsPropertyDlgCaption);
  InternalAdd('cxSCheckComboBoxEmptySelectionText', @cxSCheckComboBoxEmptySelectionText);
  InternalAdd('cxSColorComboBoxDefaultDescription', @cxSColorComboBoxDefaultDescription);
  InternalAdd('cxSEditRichEditLibraryError', @cxSEditRichEditLibraryError);
  InternalAdd('cxSEditRichEditLineInsertionError', @cxSEditRichEditLineInsertionError);
  InternalAdd('cxSEditRichEditLoadFail', @cxSEditRichEditLoadFail);
  InternalAdd('cxSEditRichEditSaveFail', @cxSEditRichEditSaveFail);
  InternalAdd('cxSEditRichEditSelectionSaveFail', @cxSEditRichEditSelectionSaveFail);
  InternalAdd('cxSEditRichEditOleInterfaceFail', @cxSEditRichEditOleInterfaceFail);
  InternalAdd('cxSEditRichEditCallBackFail', @cxSEditRichEditCallBackFail);
  InternalAdd('cxSEditRichEditLinkFail', @cxSEditRichEditLinkFail);
  InternalAdd('scxSEditRepositoryRichEditItem', @scxSEditRepositoryRichEditItem);
  InternalAdd('cxSEditRichEditUndoCaption', @cxSEditRichEditUndoCaption);
  InternalAdd('cxSEditRichEditRedoCaption', @cxSEditRichEditRedoCaption);
  InternalAdd('cxSEditRichEditCutCaption', @cxSEditRichEditCutCaption);
  InternalAdd('cxSEditRichEditCopyCaption', @cxSEditRichEditCopyCaption);
  InternalAdd('cxSEditRichEditPasteCaption', @cxSEditRichEditPasteCaption);
  InternalAdd('cxSEditRichEditDeleteCaption', @cxSEditRichEditDeleteCaption);
  InternalAdd('cxSEditRichEditSelectAllCaption', @cxSEditRichEditSelectAllCaption);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressEditorsEx Library', @AddcxExtEditResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressEditorsEx Library');

end.

