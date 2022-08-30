//---------------------------------------------------------------------------

#ifndef BarCodeDemoMainH
#define BarCodeDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include <Menus.hpp>
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxPC.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxBarCode.hpp"
#include "dxBevel.hpp"
#include "dxToggleSwitch.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TdxBarCodeDemoForm : public TfmBaseForm
{
__published:	// IDE-managed Components
	TcxTabControl *tcMain;
	TdxBevel *bvlSeparator;
	TcxGroupBox *gbSettings;
	TcxLabel *lbCodeText;
	TcxMemo *memText;
	TPanel *Panel1;
	TdxBarCode *BarCode;
	TcxPageControl *pcSettings;
	TcxTabSheet *tsBaseProperties;
    	TcxTabSheet *tsCustomProperties;
	TcxGroupBox *gbFontSize;
	TcxLabel *lbFontSize;
	TcxSpinEdit *seFontSize;
	TcxGroupBox *gbRotationAngle;
	TcxLabel *lbRotationAngle;
	TcxComboBox *cbRotationAngle;
	TcxGroupBox *gbModuleWidth;
	TcxLabel *lbModuleWidth;
	TcxSpinEdit *seModuleWidth;
	TcxGroupBox *gbFitMode;
	TcxLabel *lbModuleHeight;
	TcxComboBox *cbFitMode;
	TcxGroupBox *gbShowText;
	TcxLabel *lbShowText;
	TdxToggleSwitch *tsShowText;
	TcxGroupBox *gbWideNarrowRatio;
	TcxLabel *lbWideNarrowRatio;
	TcxSpinEdit *seWideNarrowRatio;
	TcxGroupBox *gbCalculateCheckSum;
	TcxLabel *lbCalculateCheckSum;
	TdxToggleSwitch *tsCalculateCheckSum;
	TcxGroupBox *gbCharacterSet;
	TcxLabel *lbCharacterSet;
	TcxComboBox *cbCharacterSet;
	TcxGroupBox *gbCompactionMode;
	TcxLabel *lbCompactionMode;
	TcxComboBox *cbCompactionMode;
	TcxGroupBox *gbErrorCorrectionLevel;
	TcxLabel *lbErrorCorrectionLevel;
	TcxComboBox *cbErrorCorrectionLevel;
	TcxGroupBox *gbSizeVersion;
	TcxLabel *lbSizeVersion;
	TcxComboBox *cbSizeVersion;
	void __fastcall tcMainChange(TObject *Sender);
	void __fastcall memTextPropertiesChange(TObject *Sender);
	void __fastcall seFontSizePropertiesChange(TObject *Sender);
	void __fastcall cbRotationAnglePropertiesChange(TObject *Sender);
	void __fastcall seModuleWidthPropertiesChange(TObject *Sender);
	void __fastcall cbFitModePropertiesChange(TObject *Sender);
	void __fastcall tsShowTextPropertiesChange(TObject *Sender);
	void __fastcall seWideNarrowRatioPropertiesChange(TObject *Sender);
	void __fastcall tsCalculateCheckSumPropertiesChange(TObject *Sender);
	void __fastcall cbCharacterSetPropertiesChange(TObject *Sender);
	void __fastcall cbCompactionModePropertiesChange(TObject *Sender);
	void __fastcall cbErrorCorrectionLevelPropertiesChange(TObject *Sender);
	void __fastcall cbSizeVersionPropertiesChange(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
	Boolean FLoading;
	String FIndexToSymbologyClassName[13];

	void SetWideNarrowRatio();
	void SetCalculateCheckSum();
public:		// User declarations
	__fastcall TdxBarCodeDemoForm(TComponent* Owner);
	void InitializeGenerateMessagesTimer(int ACount, Cardinal AInterval);
};
//---------------------------------------------------------------------------
extern PACKAGE TdxBarCodeDemoForm *dxBarCodeDemoForm;
//---------------------------------------------------------------------------
#endif
