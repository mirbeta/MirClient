//---------------------------------------------------------------------------

#ifndef ActivityIndicatorDemoMainH
#define ActivityIndicatorDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "dxCore.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxListView.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxActivityIndicator.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <StrUtils.hpp>
#include <Menus.hpp>
#include "cxColorComboBox.hpp"
#include "cxDropDownEdit.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxMaskEdit.hpp"
#include "cxPC.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxBevel.hpp"

//---------------------------------------------------------------------------
class TdxActivityIndicatorDemoForm : public TfmBaseForm
{
__published:	// IDE-managed Components
    TdxActivityIndicator* ActivityIndicator;
    TdxBevel* bvlSeparator;
    TcxColorComboBox* ccbArcColor;
    TcxColorComboBox* ccbDotColor;
    TcxGroupBox* gbArcBased;
    TcxGroupBox* gbCommon;
    TcxGroupBox* gbDotBased;
    TcxGroupBox* gbSettings;
    TcxLabel* lbAnimationTime;
    TcxLabel* lbArcColor;
    TcxLabel* lbArcThickness;
    TcxLabel* lbDotColor;
    TcxLabel* lbDotCount;
    TcxLabel* lbDotSize;
    TcxSpinEdit* seAnimationTime;
    TcxSpinEdit* seArcThickness;
    TcxSpinEdit* seDotCount;
    TcxSpinEdit* seDotSize;
    TcxTabControl* tcMain;
	
    void __fastcall FormCreate(TObject* Sender);
    void __fastcall seAnimationTimePropertiesChange(TObject* Sender);
    void __fastcall seArcPropertiesChange(TObject* Sender);
    void __fastcall seDotPropertiesChange(TObject* Sender);
    void __fastcall tcMainChange(TObject* Sender);
private:	// User declarations
	bool FLoading;

public:		// User declarations
	__fastcall TdxActivityIndicatorDemoForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdxActivityIndicatorDemoForm *dxActivityIndicatorDemoForm;
//---------------------------------------------------------------------------
#endif
