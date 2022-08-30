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

unit cxEditConsts;

{$I cxVer.inc}

interface

uses
  cxLibraryConsts;

resourcestring
  // Common
  cxSEditButtonCancel = 'Cancel';
  cxSEditButtonOK = 'OK';
  cxSEditDateConvertError = 'Could not convert to date';
  cxSEditInvalidRepositoryItem = 'The repository item is not acceptable';
  cxSEditNumericValueConvertError = 'Could not convert to numeric value';
  cxSEditPopupCircularReferencingError = SCircularReferencingError;
  cxSEditPostError = 'An error occurred during posting edit value';
  cxSEditTimeConvertError = 'Could not convert to time';
  cxSEditValidateErrorText = 'Invalid input value. Use escape key to abandon changes';
  cxSEditValueOutOfBounds = 'Value out of bounds';

  cxSEditCheckBoxChecked             = 'True';
  cxSEditCheckBoxGrayed              = '';
  cxSEditCheckBoxUnchecked           = 'False';
  cxSRadioGroupDefaultCaption        = '';

  cxSTextTrue                        = 'True';
  cxSTextFalse                       = 'False';

  // Combo
  cxSNoMatchesFound                  = 'No matches found';

  // Blob
  cxSBlobButtonOK                    = '&OK';
  cxSBlobButtonCancel                = '&Cancel';
  cxSBlobButtonClose                 = '&Close';
  cxSBlobMemo                        = '(MEMO)';
  cxSBlobMemoEmpty                   = '(memo)';
  cxSBlobPicture                     = '(PICTURE)';
  cxSBlobPictureEmpty                = '(picture)';

  // popup menu items
  cxSMenuItemCaptionCut              = 'Cu&t';
  cxSMenuItemCaptionCopy             = '&Copy';
  cxSMenuItemCaptionPaste            = '&Paste';
  cxSMenuItemCaptionDelete           = '&Delete';
  cxSMenuItemCaptionLoad             = '&Load...';
  cxSMenuItemCaptionSave             = 'Save &As...';
  cxSMenuItemCaptionAssignFromWebCam = 'Assign From Ca&mera...';

  // Date
  cxSDatePopupClear                  = 'Clear';
  cxSDatePopupNow                    = 'Now';
  cxSDatePopupOK                     = 'OK';
  cxSDatePopupToday                  = 'Today';
  cxSDatePopupCancel                 = 'Cancel';
  cxSDateError                       = 'Invalid Date';

  // Calculator
  scxSCalcError                      = 'Error';

  // HyperLink
  scxSHyperLinkPrefix                = 'http://';
  scxSHyperLinkDoubleSlash           = '//';

  // Navigator
  cxNavigatorHint_First = 'First record';
  cxNavigatorHint_Prior = 'Prior record';
  cxNavigatorHint_PriorPage = 'Prior page';
  cxNavigatorHint_Next = 'Next record';
  cxNavigatorHint_NextPage = 'Next page';
  cxNavigatorHint_Last = 'Last record';
  cxNavigatorHint_Insert = 'Insert record';
  cxNavigatorHint_Append = 'Append record';
  cxNavigatorHint_Delete = 'Delete record';
  cxNavigatorHint_Edit = 'Edit record';
  cxNavigatorHint_Post = 'Post edit';
  cxNavigatorHint_Cancel = 'Cancel edit';
  cxNavigatorHint_Refresh = 'Refresh data';
  cxNavigatorHint_SaveBookmark = 'Save Bookmark';
  cxNavigatorHint_GotoBookmark = 'Goto Bookmark';
  cxNavigatorHint_Filter = 'Filter data';
  cxNavigator_DeleteRecordQuestion = 'Delete record?';
  cxNavigatorInfoPanelDefaultDisplayMask = '[RecordIndex] of [RecordCount]';

  // BreadcrumbEdit
  sdxBreadcrumbEditInvalidPath = 'Cannot find "%s". Check the spelling and try again.';
  sdxBreadcrumbEditInvalidStreamVersion = 'Invalid data version: %d';

  // Edit Repository
  scxSEditRepositoryBlobItem         = 'BlobEdit|Represents an editor of BLOB (Binary Large OBject) data';
  scxSEditRepositoryButtonItem       = 'ButtonEdit|Represents an editor with embedded buttons';
  scxSEditRepositoryCalcItem         = 'CalcEdit|Represents an editor with a dropdown calculator window';
  scxSEditRepositoryCheckBoxItem     = 'CheckBox|Represents a check box control that allows selecting an option';
  scxSEditRepositoryComboBoxItem     = 'ComboBox|Represents the combo box editor';
  scxSEditRepositoryCurrencyItem     = 'CurrencyEdit|Represents an editor enabling editing currency data';
  scxSEditRepositoryDateItem         = 'DateEdit|Represents an editor with a dropdown calendar';
  scxSEditRepositoryHyperLinkItem    = 'HyperLink|Represents a text editor with hyperlink functionality';
  scxSEditRepositoryImageComboBoxItem = 'ImageComboBox|Represents an editor displaying the list of images and text strings within the dropdown window';
  scxSEditRepositoryImageItem        = 'Image|Represents an image editor';
  scxSEditRepositoryLookupComboBoxItem = 'LookupComboBox|Represents a lookup combo box control';
  scxSEditRepositoryMaskItem         = 'MaskEdit|Represents a generic masked editor.';
  scxSEditRepositoryMemoItem         = 'Memo|Represents an editor that allows editing memo data';
  scxSEditRepositoryMRUItem          = 'MRUEdit|Represents a text editor displaying the list of most recently used items (MRU) within a dropdown window';
  scxSEditRepositoryPopupItem        = 'PopupEdit|Represents an editor with a dropdown list';
  scxSEditRepositorySpinItem         = 'SpinEdit|Represents a spin editor';
  scxSEditRepositoryRadioGroupItem   = 'RadioGroup|Represents a group of radio buttons';
  scxSEditRepositoryTextItem         = 'TextEdit|Represents a single line text editor';
  scxSEditRepositoryTimeItem         = 'TimeEdit|Represents an editor displaying time values';
  scxSEditRepositoryToggleSwitchItem = 'ToggleSwitch|Represents a toggle switch editor that allows selecting an option';
  scxSEditRepositoryDateTimeWheelPickerItem = 'DateTimeWheelPicker|Represents an editor that allows the selection of a date-time value using a wheel picker';
  scxSEditRepositorySparklineItem    = 'Sparkline|Represents a sparkline editor';
  scxSEditRepositoryLookupSparklineItem = 'LookupSparkline|Represents a lookup sparkline editor';
  scxSEditRepositoryNumericWheelPickerItem = 'NumericWheelPicker|Represents an editor that allows the selection of a numeric value using a wheel picker';
  scxSEditRepositoryBarCodeItem    = 'BarCode|Represents a barcode editor';
  scxSEditRepositoryTokenItem      = 'TokenEdit|Represents an editor that visualizes text list items as simpler tokens';

  // Regular Expressions
  scxRegExprLine = 'Line';
  scxRegExprChar = 'Char';
  scxRegExprNotAssignedSourceStream = 'The source stream is not assigned';
  scxRegExprEmptySourceStream = 'The source stream is empty';
  scxRegExprCantUsePlusQuantifier = 'The ''+'' quantifier cannot be applied here';
  scxRegExprCantUseStarQuantifier = 'The ''*'' quantifier cannot be applied here';
  scxRegExprCantCreateEmptyAlt = 'The alternative should not be empty';
  scxRegExprCantCreateEmptyBlock = 'The block should not be empty';
  scxRegExprIllegalSymbol = 'Illegal ''%s''';
  scxRegExprIllegalQuantifier = 'Illegal quantifier ''%s''';
  scxRegExprNotSupportQuantifier = 'The parameter quantifiers are not supported';
  scxRegExprIllegalIntegerValue = 'Illegal integer value';
  scxRegExprTooBigReferenceNumber = 'Too big reference number';
  scxRegExprCantCreateEmptyEnum = 'Can''t create empty enumeration';
  scxRegExprSubrangeOrder = 'The starting character of the subrange must be less than the finishing one';
  scxRegExprHexNumberExpected0 = 'Hexadecimal number expected';
  scxRegExprHexNumberExpected = 'Hexadecimal number expected but ''%s'' found';
  scxRegExprMissing = 'Missing ''%s''';
  scxRegExprUnnecessary = 'Unnecessary ''%s''';
  scxRegExprIncorrectSpace = 'The space character is not allowed after ''\''';
  scxRegExprNotCompiled = 'Regular expression is not compiled';
  scxRegExprIncorrectParameterQuantifier = 'Incorrect parameter quantifier';
  scxRegExprCantUseParameterQuantifier = 'The parameter quantifier cannot be applied here';

  // MaskEdit
  scxMaskEditRegExprError = 'Regular expression errors:';
  scxMaskEditInvalidEditValue = 'The edit value is invalid';
  scxMaskEditNoMask = 'None';
  scxMaskEditIllegalFileFormat = 'Illegal file format';
  scxMaskEditEmptyMaskCollectionFile = 'The mask collection file is empty';
  scxMaskEditMaskCollectionFiles = 'Mask collection files';
  cxSSpinEditInvalidNumericValue = 'Invalid numeric value';

  // AlertWindow
  sdxAlertWindowNavigationPanelDefaultDisplayMask = '[MessageIndex] of [MessageCount]';
  sdxAlertWindowPreviousMessage = 'Previous message';
  sdxAlertWindowNextMessage = 'Next message';
  sdxAlertWindowPin = 'Pin';
  sdxAlertWindowClose = 'Close';
  sdxAlertWindowDropdown = 'Show Drop-down Menu';

  // ColorGallery
  sdxColorGalleryThemeColors = 'Theme Colors';
  sdxColorGalleryStandardColors = 'Standard Colors';

  // ColorDialog
  sdxColorDialogAddToCustomColors = '&Add to Custom Colors';
  sdxColorDialogApply = '&OK';
  sdxColorDialogCancel = '&Cancel';
  sdxColorDialogDefineCustomColor = '&Define Custom Colors >>';
  sdxColorDialogBasicColors = 'Basic Colors';
  sdxColorDialogCaption = 'Color Editor';
  sdxColorDialogCustomColors = 'Custom Colors';

  // ColorPicker
  sdxColorPickerAlphaLabel = 'A:';
  sdxColorPickerBlueLabel = 'B:';
  sdxColorPickerGreenLabel = 'G:';
  sdxColorPickerHexCodeLabel = '#';
  sdxColorPickerHueLabel = 'H:';
  sdxColorPickerLightnessLabel = 'L:';
  sdxColorPickerRedLabel = 'R:';
  sdxColorPickerSaturationLabel = 'S:';

  // ShellBrowser
  scxShellBrowserDlgCaption = 'Browse for Folder';
  scxShellBrowserDlgCurrentFolderCaption = 'Current Folder';

  // CameraControl
  sdxCameraDialogAssign = '&Assign';
  sdxCameraDialogCancel = '&Cancel';
  sdxCameraDialogPause = '&Pause';
  sdxCameraDialogPlay = '&Play';
  sdxCameraDialogCaption = 'Camera Preview';
  sdxCameraInactive = 'Inactive';
  sdxCameraRunning = 'Running';
  sdxCameraPaused = 'Paused';
  sdxCameraNotDetected = 'No camera detected';
  sdxCameraInitializing = 'Initializing...';
  sdxCameraIsBusy = 'Camera is inaccessible.'#13#10'Try closing other programs that might be using your camera';
  sdxCameraControlSettingsFormSettings = 'Settings';
  sdxCameraControlSettingsFormDevice = 'Device';
  sdxCameraControlSettingsFormDevices = 'Devices';
  sdxCameraControlSettingsFormResolution = 'Resolution';
  sdxCameraControlSettingsFormResolutions = 'Resolutions';

  // ToggleSwitch
  sdxDefaultToggleSwitchOffText = 'Off';
  sdxDefaultToggleSwitchOnText = 'On';

  // DateTimeWheelPicker
  sdxDateTimeWheelPickerHours = 'Hours';
  sdxDateTimeWheelPickerMinutes = 'Minutes';
  sdxDateTimeWheelPickerSeconds = 'Seconds';

  // Sparkline
  sdxSparklineNoData = '<No data>';

  // BarCode
  sdxBarCodeInvalidCharactersError = 'Invalid characters in the text';
  sdxBarCodeInvalidTextFormatError = 'Invalid text format';
  sdxBarCodeControlTooNarrowError = 'The control is too narrow to display a barcode';

  // TokenEdit
  sdxTokenEditMoreTokensCaption = '...';
  sdxTokenEditMoreTokensHint = '%d hidden token(s):'#13#10'(%s)';

  // RangeControl
  sdxRangeControlRangeIsEmpty = 'Range is empty';

  // UIAdorners
  sdxUIAdornerManagerBadOwner = 'TdxUIAdornerManager should have TWinControl as its Owner';

  //ExcelFilterValueContainer
  sdxExcelFilterFromValueText = 'From';
  sdxExcelFilterToValueText = 'To';
  sdxExcelFilterTopNBottomNValueLabel = 'Value';
  sdxExcelFilterTopNBottomNTypeLabel = 'Type';
  sdxExcelFilterCustomFilterAndOperatorLabel = 'And';
  sdxExcelFilterCustomFilterOrOperatorLabel = 'Or';
  sdxExcelFilterCustomFilterFirstConditionLabel = 'First';
  sdxExcelFilterCustomFilterSecondConditionLabel = 'Second';
  sdxExcelFilterValuesTabCaption = 'Values';
  sdxExcelFilterDateFiltersTabCaption = 'Date Filters';
  sdxExcelFilterTextFiltersTabCaption = 'Text Filters';
  sdxExcelFilterTimeFiltersTabCaption = 'Time Filters';
  sdxExcelFilterNumericFiltersTabCaption = 'Numeric Filters';
  sdxExcelFilterSelectValueHintText = 'Select a value...';
  sdxExcelFilterSelectDateHintText = 'Select a date...';
  sdxExcelFilterEnterValueHintText = 'Enter a value...';
  sdxExcelFilterSpecificDatePeriodsConditionText = 'Specific Date Periods';
  sdxExcelFilterEqualsConditionText = 'Equals';
  sdxExcelFilterDoesNotEqualConditionText = 'Does Not Equal';
  sdxExcelFilterBeginsWithConditionText = 'Begins With';
  sdxExcelFilterEndsWithConditionText = 'Ends With';
  sdxExcelFilterContainsConditionText = 'Contains';
  sdxExcelFilterDoesNotContainConditionText = 'Does Not Contain';
  sdxExcelFilterIsBlankConditionText = 'Is Blank';
  sdxExcelFilterIsNotBlankConditionText = 'Is Not Blank';
  sdxExcelFilterBetweenConditionText = 'Between';
  sdxExcelFilterGreaterThanConditionText = 'Greater Than';
  sdxExcelFilterGreaterThanOrEqualToConditionText = 'Greater Than Or Equal To';
  sdxExcelFilterLessThanConditionText = 'Less Than';
  sdxExcelFilterLessEqualThanOrEqualToConditionText = 'Less Than Or Equal To';
  sdxExcelFilterTopNConditionText = 'Top N';
  sdxExcelFilterBottomNConditionText = 'Bottom N';
  sdxExcelFilterAboveAverageConditionText = 'Above Average';
  sdxExcelFilterBelowAverageConditionText = 'Below Average';
  sdxExcelFilterBeforeConditionText = 'Before';
  sdxExcelFilterAfterConditionText = 'After';
  sdxExcelFilterYesterdayConditionText = 'Yesterday';
  sdxExcelFilterTodayConditionTypeText = 'Today';
  sdxExcelFilterTomorrowConditionText = 'Tomorrow';
  sdxExcelFilterLastWeekConditionText = 'Last Week';
  sdxExcelFilterThisWeekConditionText = 'This Week';
  sdxExcelFilterNextWeekConditionText = 'Next Week';
  sdxExcelFilterLastMonthConditionText = 'Last Month';
  sdxExcelFilterThisMonthConditionText = 'This Month';
  sdxExcelFilterNextMonthConditionText = 'Next Month';
  sdxExcelFilterLastYearConditionText = 'Last Year';
  sdxExcelFilterThisYearConditionText = 'This Year';
  sdxExcelFilterNextYearConditionText = 'Next Year';
  sdxExcelFilterCustomFilterConditionText = 'Custom Filter';
  sdxExcelFilterTopNBottomNItemsText = 'Items';
  sdxExcelFilterTopNBottomNPercentText = 'Percent';

  //FilterPopupWindow
  sdxFilterPopupWindowClearButtonCaption = 'Clear Filter';
  sdxFilterPopupWindowCloseButtonCaption = 'Close';
  sdxFilterPopupWindowCancelButtonCaption = 'Cancel';
  sdxFilterPopupWindowOKButtonCaption = 'OK';

implementation

uses
  dxCore;

procedure AddEditorsResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAddress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAddress);
  end;

begin
  InternalAdd('cxSEditButtonCancel', @cxSEditButtonCancel);
  InternalAdd('cxSEditButtonOK', @cxSEditButtonOK);
  InternalAdd('cxSEditDateConvertError', @cxSEditDateConvertError);
  InternalAdd('cxSEditInvalidRepositoryItem', @cxSEditInvalidRepositoryItem);
  InternalAdd('cxSEditNumericValueConvertError', @cxSEditNumericValueConvertError);
  InternalAdd('cxSEditPopupCircularReferencingError', @cxSEditPopupCircularReferencingError);
  InternalAdd('cxSEditPostError', @cxSEditPostError);
  InternalAdd('cxSEditTimeConvertError', @cxSEditTimeConvertError);
  InternalAdd('cxSEditValidateErrorText', @cxSEditValidateErrorText);
  InternalAdd('cxSEditValueOutOfBounds', @cxSEditValueOutOfBounds);
  InternalAdd('cxSEditCheckBoxChecked', @cxSEditCheckBoxChecked);
  InternalAdd('cxSEditCheckBoxGrayed', @cxSEditCheckBoxGrayed);
  InternalAdd('cxSEditCheckBoxUnchecked', @cxSEditCheckBoxUnchecked);
  InternalAdd('cxSRadioGroupDefaultCaption', @cxSRadioGroupDefaultCaption);
  InternalAdd('cxSTextTrue', @cxSTextTrue);
  InternalAdd('cxSTextFalse', @cxSTextFalse);
  InternalAdd('cxSNoMatchesFound', @cxSNoMatchesFound);
  InternalAdd('cxSBlobButtonOK', @cxSBlobButtonOK);
  InternalAdd('cxSBlobButtonCancel', @cxSBlobButtonCancel);
  InternalAdd('cxSBlobButtonClose', @cxSBlobButtonClose);
  InternalAdd('cxSBlobMemo', @cxSBlobMemo);
  InternalAdd('cxSBlobMemoEmpty', @cxSBlobMemoEmpty);
  InternalAdd('cxSBlobPicture', @cxSBlobPicture);
  InternalAdd('cxSBlobPictureEmpty', @cxSBlobPictureEmpty);
  InternalAdd('cxSMenuItemCaptionCut', @cxSMenuItemCaptionCut);
  InternalAdd('cxSMenuItemCaptionCopy', @cxSMenuItemCaptionCopy);
  InternalAdd('cxSMenuItemCaptionPaste', @cxSMenuItemCaptionPaste);
  InternalAdd('cxSMenuItemCaptionDelete', @cxSMenuItemCaptionDelete);
  InternalAdd('cxSMenuItemCaptionLoad', @cxSMenuItemCaptionLoad);
  InternalAdd('cxSMenuItemCaptionSave', @cxSMenuItemCaptionSave);
  InternalAdd('cxSMenuItemCaptionAssignFromWebCam', @cxSMenuItemCaptionAssignFromWebCam);
  InternalAdd('cxSDatePopupClear', @cxSDatePopupClear);
  InternalAdd('cxSDatePopupNow', @cxSDatePopupNow);
  InternalAdd('cxSDatePopupOK', @cxSDatePopupOK);
  InternalAdd('cxSDatePopupToday', @cxSDatePopupToday);
  InternalAdd('cxSDatePopupCancel', @cxSDatePopupCancel);
  InternalAdd('cxSDateError', @cxSDateError);
  InternalAdd('scxSCalcError', @scxSCalcError);
  InternalAdd('scxSHyperLinkPrefix', @scxSHyperLinkPrefix);
  InternalAdd('scxSHyperLinkDoubleSlash', @scxSHyperLinkDoubleSlash);
  InternalAdd('cxNavigatorHint_First', @cxNavigatorHint_First);
  InternalAdd('cxNavigatorHint_Prior', @cxNavigatorHint_Prior);
  InternalAdd('cxNavigatorHint_PriorPage', @cxNavigatorHint_PriorPage);
  InternalAdd('cxNavigatorHint_Next', @cxNavigatorHint_Next);
  InternalAdd('cxNavigatorHint_NextPage', @cxNavigatorHint_NextPage);
  InternalAdd('cxNavigatorHint_Last', @cxNavigatorHint_Last);
  InternalAdd('cxNavigatorHint_Insert', @cxNavigatorHint_Insert);
  InternalAdd('cxNavigatorHint_Append', @cxNavigatorHint_Append);
  InternalAdd('cxNavigatorHint_Delete', @cxNavigatorHint_Delete);
  InternalAdd('cxNavigatorHint_Edit', @cxNavigatorHint_Edit);
  InternalAdd('cxNavigatorHint_Post', @cxNavigatorHint_Post);
  InternalAdd('cxNavigatorHint_Cancel', @cxNavigatorHint_Cancel);
  InternalAdd('cxNavigatorHint_Refresh', @cxNavigatorHint_Refresh);
  InternalAdd('cxNavigatorHint_SaveBookmark', @cxNavigatorHint_SaveBookmark);
  InternalAdd('cxNavigatorHint_GotoBookmark', @cxNavigatorHint_GotoBookmark);
  InternalAdd('cxNavigatorHint_Filter', @cxNavigatorHint_Filter);
  InternalAdd('cxNavigator_DeleteRecordQuestion', @cxNavigator_DeleteRecordQuestion);
  InternalAdd('cxNavigatorInfoPanelDefaultDisplayMask', @cxNavigatorInfoPanelDefaultDisplayMask);
  InternalAdd('scxSEditRepositoryBlobItem', @scxSEditRepositoryBlobItem);
  InternalAdd('scxSEditRepositoryButtonItem', @scxSEditRepositoryButtonItem);
  InternalAdd('scxSEditRepositoryCalcItem', @scxSEditRepositoryCalcItem);
  InternalAdd('scxSEditRepositoryCheckBoxItem', @scxSEditRepositoryCheckBoxItem);
  InternalAdd('scxSEditRepositoryComboBoxItem', @scxSEditRepositoryComboBoxItem);
  InternalAdd('scxSEditRepositoryCurrencyItem', @scxSEditRepositoryCurrencyItem);
  InternalAdd('scxSEditRepositoryDateItem', @scxSEditRepositoryDateItem);
  InternalAdd('scxSEditRepositoryHyperLinkItem', @scxSEditRepositoryHyperLinkItem);
  InternalAdd('scxSEditRepositoryImageComboBoxItem', @scxSEditRepositoryImageComboBoxItem);
  InternalAdd('scxSEditRepositoryImageItem', @scxSEditRepositoryImageItem);
  InternalAdd('scxSEditRepositoryLookupComboBoxItem', @scxSEditRepositoryLookupComboBoxItem);
  InternalAdd('scxSEditRepositoryMaskItem', @scxSEditRepositoryMaskItem);
  InternalAdd('scxSEditRepositoryMemoItem', @scxSEditRepositoryMemoItem);
  InternalAdd('scxSEditRepositoryMRUItem', @scxSEditRepositoryMRUItem);
  InternalAdd('scxSEditRepositoryPopupItem', @scxSEditRepositoryPopupItem);
  InternalAdd('scxSEditRepositorySpinItem', @scxSEditRepositorySpinItem);
  InternalAdd('scxSEditRepositoryRadioGroupItem', @scxSEditRepositoryRadioGroupItem);
  InternalAdd('scxSEditRepositoryTextItem', @scxSEditRepositoryTextItem);
  InternalAdd('scxSEditRepositoryTimeItem', @scxSEditRepositoryTimeItem);
  InternalAdd('scxSEditRepositoryToggleSwitchItem', @scxSEditRepositoryToggleSwitchItem);
  InternalAdd('scxSEditRepositoryDateTimeWheelPickerItem', @scxSEditRepositoryDateTimeWheelPickerItem);
  InternalAdd('scxSEditRepositorySparklineItem', @scxSEditRepositorySparklineItem);
  InternalAdd('scxSEditRepositoryLookupSparklineItem', @scxSEditRepositoryLookupSparklineItem);
  InternalAdd('scxSEditRepositoryBarCodeItem', @scxSEditRepositoryBarCodeItem);
  InternalAdd('scxSEditRepositoryTokenItem', @scxSEditRepositoryTokenItem);
  InternalAdd('scxSEditRepositoryNumericWheelPickerItem', @scxSEditRepositoryNumericWheelPickerItem);
  InternalAdd('scxRegExprLine', @scxRegExprLine);
  InternalAdd('scxRegExprChar', @scxRegExprChar);
  InternalAdd('scxRegExprNotAssignedSourceStream', @scxRegExprNotAssignedSourceStream);
  InternalAdd('scxRegExprEmptySourceStream', @scxRegExprEmptySourceStream);
  InternalAdd('scxRegExprCantUsePlusQuantifier', @scxRegExprCantUsePlusQuantifier);
  InternalAdd('scxRegExprCantUseStarQuantifier', @scxRegExprCantUseStarQuantifier);
  InternalAdd('scxRegExprCantCreateEmptyAlt', @scxRegExprCantCreateEmptyAlt);
  InternalAdd('scxRegExprCantCreateEmptyBlock', @scxRegExprCantCreateEmptyBlock);
  InternalAdd('scxRegExprIllegalSymbol', @scxRegExprIllegalSymbol);
  InternalAdd('scxRegExprIllegalQuantifier', @scxRegExprIllegalQuantifier);
  InternalAdd('scxRegExprNotSupportQuantifier', @scxRegExprNotSupportQuantifier);
  InternalAdd('scxRegExprIllegalIntegerValue', @scxRegExprIllegalIntegerValue);
  InternalAdd('scxRegExprTooBigReferenceNumber', @scxRegExprTooBigReferenceNumber);
  InternalAdd('scxRegExprCantCreateEmptyEnum', @scxRegExprCantCreateEmptyEnum);
  InternalAdd('scxRegExprSubrangeOrder', @scxRegExprSubrangeOrder);
  InternalAdd('scxRegExprHexNumberExpected0', @scxRegExprHexNumberExpected0);
  InternalAdd('scxRegExprHexNumberExpected', @scxRegExprHexNumberExpected);
  InternalAdd('scxRegExprMissing', @scxRegExprMissing);
  InternalAdd('scxRegExprUnnecessary', @scxRegExprUnnecessary);
  InternalAdd('scxRegExprIncorrectSpace', @scxRegExprIncorrectSpace);
  InternalAdd('scxRegExprNotCompiled', @scxRegExprNotCompiled);
  InternalAdd('scxRegExprIncorrectParameterQuantifier', @scxRegExprIncorrectParameterQuantifier);
  InternalAdd('scxRegExprCantUseParameterQuantifier', @scxRegExprCantUseParameterQuantifier);
  InternalAdd('scxMaskEditRegExprError', @scxMaskEditRegExprError);
  InternalAdd('scxMaskEditInvalidEditValue', @scxMaskEditInvalidEditValue);
  InternalAdd('scxMaskEditNoMask', @scxMaskEditNoMask);
  InternalAdd('scxMaskEditIllegalFileFormat', @scxMaskEditIllegalFileFormat);
  InternalAdd('scxMaskEditEmptyMaskCollectionFile', @scxMaskEditEmptyMaskCollectionFile);
  InternalAdd('scxMaskEditMaskCollectionFiles', @scxMaskEditMaskCollectionFiles);
  InternalAdd('cxSSpinEditInvalidNumericValue', @cxSSpinEditInvalidNumericValue);
  InternalAdd('sdxAlertWindowNavigationPanelDefaultDisplayMask', @sdxAlertWindowNavigationPanelDefaultDisplayMask);
  InternalAdd('sdxAlertWindowPreviousMessage', @sdxAlertWindowPreviousMessage);
  InternalAdd('sdxAlertWindowNextMessage', @sdxAlertWindowNextMessage);
  InternalAdd('sdxAlertWindowPin', @sdxAlertWindowPin);
  InternalAdd('sdxAlertWindowClose', @sdxAlertWindowClose);
  InternalAdd('sdxAlertWindowDropdown', @sdxAlertWindowDropdown);
  InternalAdd('sdxBreadcrumbEditInvalidPath', @sdxBreadcrumbEditInvalidPath);
  InternalAdd('sdxBreadcrumbEditInvalidStreamVersion', @sdxBreadcrumbEditInvalidStreamVersion);
  InternalAdd('sdxColorGalleryThemeColors', @sdxColorGalleryThemeColors);
  InternalAdd('sdxColorGalleryStandardColors', @sdxColorGalleryStandardColors);
  InternalAdd('sdxColorDialogAddToCustomColors', @sdxColorDialogAddToCustomColors);
  InternalAdd('sdxColorDialogApply', @sdxColorDialogApply);
  InternalAdd('sdxColorDialogCancel', @sdxColorDialogCancel);
  InternalAdd('sdxColorDialogDefineCustomColor', @sdxColorDialogDefineCustomColor);
  InternalAdd('sdxColorDialogBasicColors', @sdxColorDialogBasicColors);
  InternalAdd('sdxColorDialogCustomColors', @sdxColorDialogCustomColors);
  InternalAdd('sdxColorDialogCaption', @sdxColorDialogCaption);
  InternalAdd('scxShellBrowserDlgCaption', @scxShellBrowserDlgCaption);
  InternalAdd('scxShellBrowserDlgCurrentFolderCaption', @scxShellBrowserDlgCurrentFolderCaption);
  InternalAdd('sdxColorPickerAlphaLabel', @sdxColorPickerAlphaLabel);
  InternalAdd('sdxColorPickerBlueLabel', @sdxColorPickerBlueLabel);
  InternalAdd('sdxColorPickerGreenLabel', @sdxColorPickerGreenLabel);
  InternalAdd('sdxColorPickerHexCodeLabel', @sdxColorPickerHexCodeLabel);
  InternalAdd('sdxColorPickerHueLabel', @sdxColorPickerHueLabel);
  InternalAdd('sdxColorPickerLightnessLabel', @sdxColorPickerLightnessLabel);
  InternalAdd('sdxColorPickerRedLabel', @sdxColorPickerRedLabel);
  InternalAdd('sdxColorPickerSaturationLabel', @sdxColorPickerSaturationLabel);
  InternalAdd('sdxCameraDialogAssign', @sdxCameraDialogAssign);
  InternalAdd('sdxCameraDialogCancel', @sdxCameraDialogCancel);
  InternalAdd('sdxCameraDialogPause', @sdxCameraDialogPause);
  InternalAdd('sdxCameraDialogPlay', @sdxCameraDialogPlay);
  InternalAdd('sdxCameraDialogCaption', @sdxCameraDialogCaption);
  InternalAdd('sdxCameraInactive', @sdxCameraInactive);
  InternalAdd('sdxCameraRunning', @sdxCameraRunning);
  InternalAdd('sdxCameraPaused', @sdxCameraPaused);
  InternalAdd('sdxCameraNotDetected', @sdxCameraNotDetected);
  InternalAdd('sdxCameraInitializing', @sdxCameraInitializing);
  InternalAdd('sdxCameraIsBusy', @sdxCameraIsBusy);
  InternalAdd('sdxCameraControlSettingsFormSettings', @sdxCameraControlSettingsFormSettings);
  InternalAdd('sdxCameraControlSettingsFormDevice', @sdxCameraControlSettingsFormDevice);
  InternalAdd('sdxCameraControlSettingsFormDevices', @sdxCameraControlSettingsFormDevices);
  InternalAdd('sdxCameraControlSettingsFormResolution', @sdxCameraControlSettingsFormResolution);
  InternalAdd('sdxCameraControlSettingsFormResolutions', @sdxCameraControlSettingsFormResolutions);
  InternalAdd('sdxDefaultToggleSwitchOffText', @sdxDefaultToggleSwitchOffText);
  InternalAdd('sdxDefaultToggleSwitchOnText', @sdxDefaultToggleSwitchOnText);
  InternalAdd('sdxDateTimeWheelPickerHours', @sdxDateTimeWheelPickerHours);
  InternalAdd('sdxDateTimeWheelPickerMinutes', @sdxDateTimeWheelPickerMinutes);
  InternalAdd('sdxDateTimeWheelPickerSeconds', @sdxDateTimeWheelPickerSeconds);
  InternalAdd('sdxSparklineNoData', @sdxSparklineNoData);

  InternalAdd('sdxBarCodeInvalidCharactersError', @sdxBarCodeInvalidCharactersError);
  InternalAdd('sdxBarCodeInvalidTextFormatError', @sdxBarCodeInvalidTextFormatError);
  InternalAdd('sdxBarCodeControlTooNarrowError', @sdxBarCodeControlTooNarrowError);

  InternalAdd('sdxTokenEditMoreTokensCaption', @sdxTokenEditMoreTokensCaption);
  InternalAdd('sdxTokenEditMoreTokensHint', @sdxTokenEditMoreTokensHint);
  InternalAdd('sdxRangeControlRangeIsEmpty', @sdxRangeControlRangeIsEmpty);

  InternalAdd('sdxExcelFilterFromValueText', @sdxExcelFilterFromValueText);
  InternalAdd('sdxExcelFilterToValueText', @sdxExcelFilterToValueText);
  InternalAdd('sdxExcelFilterTopNBottomNValueLabel', @sdxExcelFilterTopNBottomNValueLabel);
  InternalAdd('sdxExcelFilterTopNBottomNTypeLabel', @sdxExcelFilterTopNBottomNTypeLabel);
  InternalAdd('sdxExcelFilterCustomFilterAndOperatorLabel', @sdxExcelFilterCustomFilterAndOperatorLabel);
  InternalAdd('sdxExcelFilterCustomFilterOrOperatorLabel', @sdxExcelFilterCustomFilterOrOperatorLabel);
  InternalAdd('sdxExcelFilterCustomFilterFirstConditionLabel', @sdxExcelFilterCustomFilterFirstConditionLabel);
  InternalAdd('sdxExcelFilterCustomFilterSecondConditionLabel', @sdxExcelFilterCustomFilterSecondConditionLabel);
  InternalAdd('sdxExcelFilterValuesTabCaption', @sdxExcelFilterValuesTabCaption);
  InternalAdd('sdxExcelFilterDateFiltersTabCaption', @sdxExcelFilterDateFiltersTabCaption);
  InternalAdd('sdxExcelFilterTextFiltersTabCaption', @sdxExcelFilterTextFiltersTabCaption);
  InternalAdd('sdxExcelFilterTimeFiltersTabCaption', @sdxExcelFilterTimeFiltersTabCaption);
  InternalAdd('sdxExcelFilterNumericFiltersTabCaption', @sdxExcelFilterNumericFiltersTabCaption);
  InternalAdd('sdxExcelFilterSelectValueHintText', @sdxExcelFilterSelectValueHintText);
  InternalAdd('sdxExcelFilterSelectDateHintText', @sdxExcelFilterSelectDateHintText);
  InternalAdd('sdxExcelFilterEnterValueHintText', @sdxExcelFilterEnterValueHintText);
  InternalAdd('sdxExcelFilterSpecificDatePeriodsConditionText', @sdxExcelFilterSpecificDatePeriodsConditionText);
  InternalAdd('sdxExcelFilterEqualsConditionText', @sdxExcelFilterEqualsConditionText);
  InternalAdd('sdxExcelFilterDoesNotEqualConditionText', @sdxExcelFilterDoesNotEqualConditionText);
  InternalAdd('sdxExcelFilterBeginsWithConditionText', @sdxExcelFilterBeginsWithConditionText);
  InternalAdd('sdxExcelFilterEndsWithConditionText', @sdxExcelFilterEndsWithConditionText);
  InternalAdd('sdxExcelFilterContainsConditionText', @sdxExcelFilterContainsConditionText);
  InternalAdd('sdxExcelFilterDoesNotContainConditionText', @sdxExcelFilterDoesNotContainConditionText);
  InternalAdd('sdxExcelFilterIsBlankConditionText', @sdxExcelFilterIsBlankConditionText);
  InternalAdd('sdxExcelFilterIsNotBlankConditionText', @sdxExcelFilterIsNotBlankConditionText);
  InternalAdd('sdxExcelFilterBetweenConditionText', @sdxExcelFilterBetweenConditionText);
  InternalAdd('sdxExcelFilterGreaterThanConditionText', @sdxExcelFilterGreaterThanConditionText);
  InternalAdd('sdxExcelFilterGreaterThanOrEqualToConditionText', @sdxExcelFilterGreaterThanOrEqualToConditionText);
  InternalAdd('sdxExcelFilterLessThanConditionText', @sdxExcelFilterLessThanConditionText);
  InternalAdd('sdxExcelFilterLessEqualThanOrEqualToConditionText', @sdxExcelFilterLessEqualThanOrEqualToConditionText);
  InternalAdd('sdxExcelFilterTopNConditionText', @sdxExcelFilterTopNConditionText);
  InternalAdd('sdxExcelFilterBottomNConditionText', @sdxExcelFilterBottomNConditionText);
  InternalAdd('sdxExcelFilterAboveAverageConditionText', @sdxExcelFilterAboveAverageConditionText);
  InternalAdd('sdxExcelFilterBelowAverageConditionText', @sdxExcelFilterBelowAverageConditionText);
  InternalAdd('sdxExcelFilterBeforeConditionText', @sdxExcelFilterBeforeConditionText);
  InternalAdd('sdxExcelFilterAfterConditionText', @sdxExcelFilterAfterConditionText);
  InternalAdd('sdxExcelFilterYesterdayConditionText', @sdxExcelFilterYesterdayConditionText);
  InternalAdd('sdxExcelFilterTodayConditionTypeText', @sdxExcelFilterTodayConditionTypeText);
  InternalAdd('sdxExcelFilterTomorrowConditionText', @sdxExcelFilterTomorrowConditionText);
  InternalAdd('sdxExcelFilterLastWeekConditionText', @sdxExcelFilterLastWeekConditionText);
  InternalAdd('sdxExcelFilterThisWeekConditionText', @sdxExcelFilterThisWeekConditionText);
  InternalAdd('sdxExcelFilterNextWeekConditionText', @sdxExcelFilterNextWeekConditionText);
  InternalAdd('sdxExcelFilterLastMonthConditionText', @sdxExcelFilterLastMonthConditionText);
  InternalAdd('sdxExcelFilterThisMonthConditionText', @sdxExcelFilterThisMonthConditionText);
  InternalAdd('sdxExcelFilterNextMonthConditionText', @sdxExcelFilterNextMonthConditionText);
  InternalAdd('sdxExcelFilterLastYearConditionText', @sdxExcelFilterLastYearConditionText);
  InternalAdd('sdxExcelFilterThisYearConditionText', @sdxExcelFilterThisYearConditionText);
  InternalAdd('sdxExcelFilterNextYearConditionText', @sdxExcelFilterNextYearConditionText);
  InternalAdd('sdxExcelFilterCustomFilterConditionText', @sdxExcelFilterCustomFilterConditionText);
  InternalAdd('sdxExcelFilterTopNBottomNItemsText', @sdxExcelFilterTopNBottomNItemsText);
  InternalAdd('sdxExcelFilterTopNBottomNPercentText', @sdxExcelFilterTopNBottomNPercentText);

  InternalAdd('sdxFilterPopupWindowClearButtonCaption', @sdxFilterPopupWindowClearButtonCaption);
  InternalAdd('sdxFilterPopupWindowCloseButtonCaption', @sdxFilterPopupWindowCloseButtonCaption);
  InternalAdd('sdxFilterPopupWindowCancelButtonCaption', @sdxFilterPopupWindowCancelButtonCaption);
  InternalAdd('sdxFilterPopupWindowOKButtonCaption', @sdxFilterPopupWindowOKButtonCaption);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressEditors Library', @AddEditorsResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressEditors Library');

end.
