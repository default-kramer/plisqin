#lang racket

(provide (all-defined-out))

(require plisqin
         plisqin-examples/adventure-works)

(current-connection (connect-adventure-works))

(define-schema adventure-works-schema
  (table Department
         #:column
         [DepartmentID #:type Number]
         [GroupName #:type String]
         [ModifiedDate #:type Datetime]
         [Name #:type String])
  (table Employee
         #:column
         [BirthDate #:type Datetime]
         [BusinessEntityID #:type Number]
         [CurrentFlag]
         [Gender #:type String]
         [HireDate #:type Datetime]
         [JobTitle #:type String]
         [LoginID #:type String]
         [MaritalStatus #:type String]
         [ModifiedDate #:type Datetime]
         [NationalIDNumber #:type String]
         [OrganizationLevel #:type Number]
         [OrganizationNode]
         [rowguid]
         [SalariedFlag]
         [SickLeaveHours #:type Number]
         [VacationHours #:type Number])
  (table EmployeeDepartmentHistory
         #:column
         [BusinessEntityID #:type Number]
         [DepartmentID #:type Number]
         [EndDate #:type Datetime]
         [ModifiedDate #:type Datetime]
         [ShiftID #:type Number]
         [StartDate #:type Datetime])
  (table EmployeePayHistory
         #:column
         [BusinessEntityID #:type Number]
         [ModifiedDate #:type Datetime]
         [PayFrequency #:type Number]
         [Rate #:type Number]
         [RateChangeDate #:type Datetime])
  (table JobCandidate
         #:column
         [BusinessEntityID #:type Number]
         [JobCandidateID #:type Number]
         [ModifiedDate #:type Datetime]
         [Resume])
  (table Shift
         #:column
         [EndTime]
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [ShiftID #:type Number]
         [StartTime])
  (table Address
         #:column
         [AddressID #:type Number]
         [AddressLine1 #:type String]
         [AddressLine2 #:type String]
         [City #:type String]
         [ModifiedDate #:type Datetime]
         [PostalCode #:type String]
         [rowguid]
         [SpatialLocation]
         [StateProvinceID #:type Number])
  (table AddressType
         #:column
         [AddressTypeID #:type Number]
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [rowguid])
  (table BusinessEntity
         #:column
         [BusinessEntityID #:type Number]
         [ModifiedDate #:type Datetime]
         [rowguid])
  (table BusinessEntityAddress
         #:column
         [AddressID #:type Number]
         [AddressTypeID #:type Number]
         [BusinessEntityID #:type Number]
         [ModifiedDate #:type Datetime]
         [rowguid])
  (table BusinessEntityContact
         #:column
         [BusinessEntityID #:type Number]
         [ContactTypeID #:type Number]
         [ModifiedDate #:type Datetime]
         [PersonID #:type Number]
         [rowguid])
  (table ContactType
         #:column
         [ContactTypeID #:type Number]
         [ModifiedDate #:type Datetime]
         [Name #:type String])
  (table CountryRegion
         #:column
         [CountryRegionCode #:type String]
         [ModifiedDate #:type Datetime]
         [Name #:type String])
  (table EmailAddress
         #:column
         [BusinessEntityID #:type Number]
         [EmailAddress #:type String]
         [EmailAddressID #:type Number]
         [ModifiedDate #:type Datetime]
         [rowguid])
  (table Password
         #:column
         [BusinessEntityID #:type Number]
         [ModifiedDate #:type Datetime]
         [PasswordHash #:type String]
         [PasswordSalt #:type String]
         [rowguid])
  (table Person
         #:column
         [AdditionalContactInfo]
         [BusinessEntityID #:type Number]
         [Demographics]
         [EmailPromotion #:type Number]
         [FirstName #:type String]
         [LastName #:type String]
         [MiddleName #:type String]
         [ModifiedDate #:type Datetime]
         [NameStyle]
         [PersonType #:type String]
         [rowguid]
         [Suffix #:type String]
         [Title #:type String])
  (table PersonPhone
         #:column
         [BusinessEntityID #:type Number]
         [ModifiedDate #:type Datetime]
         [PhoneNumber #:type String]
         [PhoneNumberTypeID #:type Number])
  (table PhoneNumberType
         #:column
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [PhoneNumberTypeID #:type Number])
  (table StateProvince
         #:column
         [CountryRegionCode #:type String]
         [IsOnlyStateProvinceFlag]
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [rowguid]
         [StateProvinceCode #:type String]
         [StateProvinceID #:type Number]
         [TerritoryID #:type Number])
  (table BillOfMaterials
         #:column
         [BillOfMaterialsID #:type Number]
         [BOMLevel #:type Number]
         [ComponentID #:type Number]
         [EndDate #:type Datetime]
         [ModifiedDate #:type Datetime]
         [PerAssemblyQty #:type Number]
         [ProductAssemblyID #:type Number]
         [StartDate #:type Datetime]
         [UnitMeasureCode #:type String])
  (table Culture
         #:column
         [CultureID #:type String]
         [ModifiedDate #:type Datetime]
         [Name #:type String])
  (table Document
         #:column
         [ChangeNumber #:type Number]
         [Document]
         [DocumentLevel #:type Number]
         [DocumentNode]
         [DocumentSummary #:type String]
         [FileExtension #:type String]
         [FileName #:type String]
         [FolderFlag]
         [ModifiedDate #:type Datetime]
         [Owner #:type Number]
         [Revision #:type String]
         [rowguid]
         [Status #:type Number]
         [Title #:type String])
  (table Illustration
         #:column
         [Diagram]
         [IllustrationID #:type Number]
         [ModifiedDate #:type Datetime])
  (table Location
         #:column
         [Availability #:type Number]
         [CostRate #:type Number]
         [LocationID #:type Number]
         [ModifiedDate #:type Datetime]
         [Name #:type String])
  (table Product
         #:column
         [Class #:type String]
         [Color #:type String]
         [DaysToManufacture #:type Number]
         [DiscontinuedDate #:type Datetime]
         [FinishedGoodsFlag]
         [ListPrice #:type Number]
         [MakeFlag]
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [ProductID #:type Number]
         [ProductLine #:type String]
         [ProductModelID #:type Number]
         [ProductNumber #:type String]
         [ProductSubcategoryID #:type Number]
         [ReorderPoint #:type Number]
         [rowguid]
         [SafetyStockLevel #:type Number]
         [SellEndDate #:type Datetime]
         [SellStartDate #:type Datetime]
         [Size #:type String]
         [SizeUnitMeasureCode #:type String]
         [StandardCost #:type Number]
         [Style #:type String]
         [Weight #:type Number]
         [WeightUnitMeasureCode #:type String]
         #:has-one
         [ProductSubcategory
          (join subcat ProductSubcategory
                (join-type 'left)
                (join-on (.= (ProductSubcategoryID subcat)
                             (ProductSubcategoryID this))))]
         #:property
         [ProductName
          (Name this)]
         [SubcategoryName
          (SubcategoryName (ProductSubcategory this))]
         [CategoryName
          (CategoryName (ProductSubcategory this))])
  (table ProductCategory
         #:column
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [ProductCategoryID #:type Number]
         [rowguid]
         #:property
         [CategoryName
          (Name this)])
  (table ProductCostHistory
         #:column
         [EndDate #:type Datetime]
         [ModifiedDate #:type Datetime]
         [ProductID #:type Number]
         [StandardCost #:type Number]
         [StartDate #:type Datetime])
  (table ProductDescription
         #:column
         [Description #:type String]
         [ModifiedDate #:type Datetime]
         [ProductDescriptionID #:type Number]
         [rowguid])
  (table ProductDocument
         #:column
         [DocumentNode]
         [ModifiedDate #:type Datetime]
         [ProductID #:type Number])
  (table ProductInventory
         #:column
         [Bin #:type Number]
         [LocationID #:type Number]
         [ModifiedDate #:type Datetime]
         [ProductID #:type Number]
         [Quantity #:type Number]
         [rowguid]
         [Shelf #:type String])
  (table ProductListPriceHistory
         #:column
         [EndDate #:type Datetime]
         [ListPrice #:type Number]
         [ModifiedDate #:type Datetime]
         [ProductID #:type Number]
         [StartDate #:type Datetime])
  (table ProductModel
         #:column
         [CatalogDescription]
         [Instructions]
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [ProductModelID #:type Number]
         [rowguid])
  (table ProductModelIllustration
         #:column
         [IllustrationID #:type Number]
         [ModifiedDate #:type Datetime]
         [ProductModelID #:type Number])
  (table ProductModelProductDescriptionCulture
         #:column
         [CultureID #:type String]
         [ModifiedDate #:type Datetime]
         [ProductDescriptionID #:type Number]
         [ProductModelID #:type Number])
  (table ProductPhoto
         #:column
         [LargePhoto]
         [LargePhotoFileName #:type String]
         [ModifiedDate #:type Datetime]
         [ProductPhotoID #:type Number]
         [ThumbNailPhoto]
         [ThumbnailPhotoFileName #:type String])
  (table ProductProductPhoto
         #:column
         [ModifiedDate #:type Datetime]
         [Primary]
         [ProductID #:type Number]
         [ProductPhotoID #:type Number])
  (table ProductReview
         #:column
         [Comments #:type String]
         [EmailAddress #:type String]
         [ModifiedDate #:type Datetime]
         [ProductID #:type Number]
         [ProductReviewID #:type Number]
         [Rating #:type Number]
         [ReviewDate #:type Datetime]
         [ReviewerName #:type String])
  (table ProductSubcategory
         #:column
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [ProductCategoryID #:type Number]
         [ProductSubcategoryID #:type Number]
         [rowguid]
         #:has-one
         [ProductCategory
          (join cat ProductCategory
                (join-on (.= (ProductCategoryID cat)
                             (ProductCategoryID this))))]
         #:property
         [CategoryName
          (CategoryName (ProductCategory this))]
         [SubcategoryName
          (Name this)])
  (table ScrapReason
         #:column
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [ScrapReasonID #:type Number])
  (table TransactionHistory
         #:column
         [ActualCost #:type Number]
         [ModifiedDate #:type Datetime]
         [ProductID #:type Number]
         [Quantity #:type Number]
         [ReferenceOrderID #:type Number]
         [ReferenceOrderLineID #:type Number]
         [TransactionDate #:type Datetime]
         [TransactionID #:type Number]
         [TransactionType #:type String])
  (table TransactionHistoryArchive
         #:column
         [ActualCost #:type Number]
         [ModifiedDate #:type Datetime]
         [ProductID #:type Number]
         [Quantity #:type Number]
         [ReferenceOrderID #:type Number]
         [ReferenceOrderLineID #:type Number]
         [TransactionDate #:type Datetime]
         [TransactionID #:type Number]
         [TransactionType #:type String])
  (table UnitMeasure
         #:column
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [UnitMeasureCode #:type String])
  (table WorkOrder
         #:column
         [DueDate #:type Datetime]
         [EndDate #:type Datetime]
         [ModifiedDate #:type Datetime]
         [OrderQty #:type Number]
         [ProductID #:type Number]
         [ScrappedQty #:type Number]
         [ScrapReasonID #:type Number]
         [StartDate #:type Datetime]
         [StockedQty #:type Number]
         [WorkOrderID #:type Number])
  (table WorkOrderRouting
         #:column
         [ActualCost #:type Number]
         [ActualEndDate #:type Datetime]
         [ActualResourceHrs #:type Number]
         [ActualStartDate #:type Datetime]
         [LocationID #:type Number]
         [ModifiedDate #:type Datetime]
         [OperationSequence #:type Number]
         [PlannedCost #:type Number]
         [ProductID #:type Number]
         [ScheduledEndDate #:type Datetime]
         [ScheduledStartDate #:type Datetime]
         [WorkOrderID #:type Number])
  (table ProductVendor
         #:column
         [AverageLeadTime #:type Number]
         [BusinessEntityID #:type Number]
         [LastReceiptCost #:type Number]
         [LastReceiptDate #:type Datetime]
         [MaxOrderQty #:type Number]
         [MinOrderQty #:type Number]
         [ModifiedDate #:type Datetime]
         [OnOrderQty #:type Number]
         [ProductID #:type Number]
         [StandardPrice #:type Number]
         [UnitMeasureCode #:type String])
  (table PurchaseOrderDetail
         #:column
         [DueDate #:type Datetime]
         [LineTotal #:type Number]
         [ModifiedDate #:type Datetime]
         [OrderQty #:type Number]
         [ProductID #:type Number]
         [PurchaseOrderDetailID #:type Number]
         [PurchaseOrderID #:type Number]
         [ReceivedQty #:type Number]
         [RejectedQty #:type Number]
         [StockedQty #:type Number]
         [UnitPrice #:type Number])
  (table PurchaseOrderHeader
         #:column
         [EmployeeID #:type Number]
         [Freight #:type Number]
         [ModifiedDate #:type Datetime]
         [OrderDate #:type Datetime]
         [PurchaseOrderID #:type Number]
         [RevisionNumber #:type Number]
         [ShipDate #:type Datetime]
         [ShipMethodID #:type Number]
         [Status #:type Number]
         [SubTotal #:type Number]
         [TaxAmt #:type Number]
         [TotalDue #:type Number]
         [VendorID #:type Number])
  (table ShipMethod
         #:column
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [rowguid]
         [ShipBase #:type Number]
         [ShipMethodID #:type Number]
         [ShipRate #:type Number])
  (table Vendor
         #:column
         [AccountNumber #:type String]
         [ActiveFlag]
         [BusinessEntityID #:type Number]
         [CreditRating #:type Number]
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [PreferredVendorStatus]
         [PurchasingWebServiceURL #:type String])
  (table CountryRegionCurrency
         #:column
         [CountryRegionCode #:type String]
         [CurrencyCode #:type String]
         [ModifiedDate #:type Datetime])
  (table CreditCard
         #:column
         [CardNumber #:type String]
         [CardType #:type String]
         [CreditCardID #:type Number]
         [ExpMonth #:type Number]
         [ExpYear #:type Number]
         [ModifiedDate #:type Datetime])
  (table Currency
         #:column
         [CurrencyCode #:type String]
         [ModifiedDate #:type Datetime]
         [Name #:type String])
  (table CurrencyRate
         #:column
         [AverageRate #:type Number]
         [CurrencyRateDate #:type Datetime]
         [CurrencyRateID #:type Number]
         [EndOfDayRate #:type Number]
         [FromCurrencyCode #:type String]
         [ModifiedDate #:type Datetime]
         [ToCurrencyCode #:type String])
  (table Customer
         #:column
         [AccountNumber #:type String]
         [CustomerID #:type Number]
         [ModifiedDate #:type Datetime]
         [PersonID #:type Number]
         [rowguid]
         [StoreID #:type Number]
         [TerritoryID #:type Number])
  (table PersonCreditCard
         #:column
         [BusinessEntityID #:type Number]
         [CreditCardID #:type Number]
         [ModifiedDate #:type Datetime])
  (table SalesOrderDetail
         #:column
         [CarrierTrackingNumber #:type String]
         [LineTotal #:type Number]
         [ModifiedDate #:type Datetime]
         [OrderQty #:type Number]
         [ProductID #:type Number]
         [rowguid]
         [SalesOrderDetailID #:type Number]
         [SalesOrderID #:type Number]
         [SpecialOfferID #:type Number]
         [UnitPrice #:type Number]
         [UnitPriceDiscount #:type Number])
  (table SalesOrderHeader
         #:column
         [AccountNumber #:type String]
         [BillToAddressID #:type Number]
         [Comment #:type String]
         [CreditCardApprovalCode #:type String]
         [CreditCardID #:type Number]
         [CurrencyRateID #:type Number]
         [CustomerID #:type Number]
         [DueDate #:type Datetime]
         [Freight #:type Number]
         [ModifiedDate #:type Datetime]
         [OnlineOrderFlag]
         [OrderDate #:type Datetime]
         [PurchaseOrderNumber #:type String]
         [RevisionNumber #:type Number]
         [rowguid]
         [SalesOrderID #:type Number]
         [SalesOrderNumber #:type String]
         [SalesPersonID #:type Number]
         [ShipDate #:type Datetime]
         [ShipMethodID #:type Number]
         [ShipToAddressID #:type Number]
         [Status #:type Number]
         [SubTotal #:type Number]
         [TaxAmt #:type Number]
         [TerritoryID #:type Number]
         [TotalDue #:type Number])
  (table SalesOrderHeaderSalesReason
         #:column
         [ModifiedDate #:type Datetime]
         [SalesOrderID #:type Number]
         [SalesReasonID #:type Number])
  (table SalesPerson
         #:column
         [Bonus #:type Number]
         [BusinessEntityID #:type Number]
         [CommissionPct #:type Number]
         [ModifiedDate #:type Datetime]
         [rowguid]
         [SalesLastYear #:type Number]
         [SalesQuota #:type Number]
         [SalesYTD #:type Number]
         [TerritoryID #:type Number])
  (table SalesPersonQuotaHistory
         #:column
         [BusinessEntityID #:type Number]
         [ModifiedDate #:type Datetime]
         [QuotaDate #:type Datetime]
         [rowguid]
         [SalesQuota #:type Number])
  (table SalesReason
         #:column
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [ReasonType #:type String]
         [SalesReasonID #:type Number])
  (table SalesTaxRate
         #:column
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [rowguid]
         [SalesTaxRateID #:type Number]
         [StateProvinceID #:type Number]
         [TaxRate #:type Number]
         [TaxType #:type Number])
  (table SalesTerritory
         #:column
         [CostLastYear #:type Number]
         [CostYTD #:type Number]
         [CountryRegionCode #:type String]
         [Group #:type String]
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [rowguid]
         [SalesLastYear #:type Number]
         [SalesYTD #:type Number]
         [TerritoryID #:type Number])
  (table SalesTerritoryHistory
         #:column
         [BusinessEntityID #:type Number]
         [EndDate #:type Datetime]
         [ModifiedDate #:type Datetime]
         [rowguid]
         [StartDate #:type Datetime]
         [TerritoryID #:type Number])
  (table ShoppingCartItem
         #:column
         [DateCreated #:type Datetime]
         [ModifiedDate #:type Datetime]
         [ProductID #:type Number]
         [Quantity #:type Number]
         [ShoppingCartID #:type String]
         [ShoppingCartItemID #:type Number])
  (table SpecialOffer
         #:column
         [Category #:type String]
         [Description #:type String]
         [DiscountPct #:type Number]
         [EndDate #:type Datetime]
         [MaxQty #:type Number]
         [MinQty #:type Number]
         [ModifiedDate #:type Datetime]
         [rowguid]
         [SpecialOfferID #:type Number]
         [StartDate #:type Datetime]
         [Type #:type String])
  (table SpecialOfferProduct
         #:column
         [ModifiedDate #:type Datetime]
         [ProductID #:type Number]
         [rowguid]
         [SpecialOfferID #:type Number])
  (table Store
         #:column
         [BusinessEntityID #:type Number]
         [Demographics]
         [ModifiedDate #:type Datetime]
         [Name #:type String]
         [rowguid]
         [SalesPersonID #:type Number]))
