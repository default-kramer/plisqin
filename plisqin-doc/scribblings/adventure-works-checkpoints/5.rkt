#lang racket

(provide (all-defined-out))

(require plisqin
         plisqin-examples/adventure-works)

(current-connection (connect-adventure-works))

(define-schema adventure-works-schema
  (table Department
         #:column
         DepartmentID
         Name
         GroupName
         ModifiedDate)
  (table Employee
         #:column
         BusinessEntityID
         NationalIDNumber
         LoginID
         OrganizationNode
         OrganizationLevel
         JobTitle
         BirthDate
         MaritalStatus
         Gender
         HireDate
         SalariedFlag
         VacationHours
         SickLeaveHours
         CurrentFlag
         rowguid
         ModifiedDate)
  (table EmployeeDepartmentHistory
         #:column
         BusinessEntityID
         DepartmentID
         ShiftID
         StartDate
         EndDate
         ModifiedDate)
  (table EmployeePayHistory
         #:column
         BusinessEntityID
         RateChangeDate
         Rate
         PayFrequency
         ModifiedDate)
  (table JobCandidate
         #:column
         JobCandidateID
         BusinessEntityID
         Resume
         ModifiedDate)
  (table Shift
         #:column
         ShiftID
         Name
         StartTime
         EndTime
         ModifiedDate)
  (table Address
         #:column
         AddressID
         AddressLine1
         AddressLine2
         City
         StateProvinceID
         PostalCode
         SpatialLocation
         rowguid
         ModifiedDate)
  (table AddressType
         #:column
         AddressTypeID
         Name
         rowguid
         ModifiedDate)
  (table BusinessEntity
         #:column
         BusinessEntityID
         rowguid
         ModifiedDate)
  (table BusinessEntityAddress
         #:column
         BusinessEntityID
         AddressID
         AddressTypeID
         rowguid
         ModifiedDate)
  (table BusinessEntityContact
         #:column
         BusinessEntityID
         PersonID
         ContactTypeID
         rowguid
         ModifiedDate)
  (table ContactType
         #:column
         ContactTypeID
         Name
         ModifiedDate)
  (table CountryRegion
         #:column
         CountryRegionCode
         Name
         ModifiedDate)
  (table EmailAddress
         #:column
         BusinessEntityID
         EmailAddressID
         EmailAddress
         rowguid
         ModifiedDate)
  (table Password
         #:column
         BusinessEntityID
         PasswordHash
         PasswordSalt
         rowguid
         ModifiedDate)
  (table Person
         #:column
         BusinessEntityID
         PersonType
         NameStyle
         Title
         FirstName
         MiddleName
         LastName
         Suffix
         EmailPromotion
         AdditionalContactInfo
         Demographics
         rowguid
         ModifiedDate)
  (table PersonPhone
         #:column
         BusinessEntityID
         PhoneNumber
         PhoneNumberTypeID
         ModifiedDate)
  (table PhoneNumberType
         #:column
         PhoneNumberTypeID
         Name
         ModifiedDate)
  (table StateProvince
         #:column
         StateProvinceID
         StateProvinceCode
         CountryRegionCode
         IsOnlyStateProvinceFlag
         Name
         TerritoryID
         rowguid
         ModifiedDate)
  (table BillOfMaterials
         #:column
         BillOfMaterialsID
         ProductAssemblyID
         ComponentID
         StartDate
         EndDate
         UnitMeasureCode
         BOMLevel
         PerAssemblyQty
         ModifiedDate)
  (table Culture
         #:column
         CultureID
         Name
         ModifiedDate)
  (table Document
         #:column
         DocumentNode
         DocumentLevel
         Title
         Owner
         FolderFlag
         FileName
         FileExtension
         Revision
         ChangeNumber
         Status
         DocumentSummary
         Document
         rowguid
         ModifiedDate)
  (table Illustration
         #:column
         IllustrationID
         Diagram
         ModifiedDate)
  (table Location
         #:column
         LocationID
         Name
         CostRate
         Availability
         ModifiedDate)
  (table Product
         #:column
         ProductID
         Name
         ProductNumber
         MakeFlag
         FinishedGoodsFlag
         Color
         SafetyStockLevel
         ReorderPoint
         StandardCost
         ListPrice
         Size
         SizeUnitMeasureCode
         WeightUnitMeasureCode
         Weight
         DaysToManufacture
         ProductLine
         Class
         Style
         ProductSubcategoryID
         ProductModelID
         SellStartDate
         SellEndDate
         DiscontinuedDate
         rowguid
         ModifiedDate)
  (table ProductCategory
         #:column
         ProductCategoryID
         Name
         rowguid
         ModifiedDate)
  (table ProductCostHistory
         #:column
         ProductID
         StartDate
         EndDate
         StandardCost
         ModifiedDate)
  (table ProductDescription
         #:column
         ProductDescriptionID
         Description
         rowguid
         ModifiedDate)
  (table ProductDocument
         #:column
         ProductID
         DocumentNode
         ModifiedDate)
  (table ProductInventory
         #:column
         ProductID
         LocationID
         Shelf
         Bin
         Quantity
         rowguid
         ModifiedDate)
  (table ProductListPriceHistory
         #:column
         ProductID
         StartDate
         EndDate
         ListPrice
         ModifiedDate)
  (table ProductModel
         #:column
         ProductModelID
         Name
         CatalogDescription
         Instructions
         rowguid
         ModifiedDate)
  (table ProductModelIllustration
         #:column
         ProductModelID
         IllustrationID
         ModifiedDate)
  (table ProductModelProductDescriptionCulture
         #:column
         ProductModelID
         ProductDescriptionID
         CultureID
         ModifiedDate)
  (table ProductPhoto
         #:column
         ProductPhotoID
         ThumbNailPhoto
         ThumbnailPhotoFileName
         LargePhoto
         LargePhotoFileName
         ModifiedDate)
  (table ProductProductPhoto
         #:column
         ProductID
         ProductPhotoID
         Primary
         ModifiedDate)
  (table ProductReview
         #:column
         ProductReviewID
         ProductID
         ReviewerName
         ReviewDate
         EmailAddress
         Rating
         Comments
         ModifiedDate)
  (table ProductSubcategory
         #:column
         ProductSubcategoryID
         ProductCategoryID
         Name
         rowguid
         ModifiedDate
         #:has-one
         [ProductCategory
          (join cat ProductCategory
                (join-on (.= (ProductCategoryID cat)
                             (ProductCategoryID this))))]
         #:property
         [CategoryName
          (Name (ProductCategory this))])
  (table ScrapReason
         #:column
         ScrapReasonID
         Name
         ModifiedDate)
  (table TransactionHistory
         #:column
         TransactionID
         ProductID
         ReferenceOrderID
         ReferenceOrderLineID
         TransactionDate
         TransactionType
         Quantity
         ActualCost
         ModifiedDate)
  (table TransactionHistoryArchive
         #:column
         TransactionID
         ProductID
         ReferenceOrderID
         ReferenceOrderLineID
         TransactionDate
         TransactionType
         Quantity
         ActualCost
         ModifiedDate)
  (table UnitMeasure
         #:column
         UnitMeasureCode
         Name
         ModifiedDate)
  (table WorkOrder
         #:column
         WorkOrderID
         ProductID
         OrderQty
         StockedQty
         ScrappedQty
         StartDate
         EndDate
         DueDate
         ScrapReasonID
         ModifiedDate)
  (table WorkOrderRouting
         #:column
         WorkOrderID
         ProductID
         OperationSequence
         LocationID
         ScheduledStartDate
         ScheduledEndDate
         ActualStartDate
         ActualEndDate
         ActualResourceHrs
         PlannedCost
         ActualCost
         ModifiedDate)
  (table ProductVendor
         #:column
         ProductID
         BusinessEntityID
         AverageLeadTime
         StandardPrice
         LastReceiptCost
         LastReceiptDate
         MinOrderQty
         MaxOrderQty
         OnOrderQty
         UnitMeasureCode
         ModifiedDate)
  (table PurchaseOrderDetail
         #:column
         PurchaseOrderID
         PurchaseOrderDetailID
         DueDate
         OrderQty
         ProductID
         UnitPrice
         LineTotal
         ReceivedQty
         RejectedQty
         StockedQty
         ModifiedDate)
  (table PurchaseOrderHeader
         #:column
         PurchaseOrderID
         RevisionNumber
         Status
         EmployeeID
         VendorID
         ShipMethodID
         OrderDate
         ShipDate
         SubTotal
         TaxAmt
         Freight
         TotalDue
         ModifiedDate)
  (table ShipMethod
         #:column
         ShipMethodID
         Name
         ShipBase
         ShipRate
         rowguid
         ModifiedDate)
  (table Vendor
         #:column
         BusinessEntityID
         AccountNumber
         Name
         CreditRating
         PreferredVendorStatus
         ActiveFlag
         PurchasingWebServiceURL
         ModifiedDate)
  (table CountryRegionCurrency
         #:column
         CountryRegionCode
         CurrencyCode
         ModifiedDate)
  (table CreditCard
         #:column
         CreditCardID
         CardType
         CardNumber
         ExpMonth
         ExpYear
         ModifiedDate)
  (table Currency
         #:column
         CurrencyCode
         Name
         ModifiedDate)
  (table CurrencyRate
         #:column
         CurrencyRateID
         CurrencyRateDate
         FromCurrencyCode
         ToCurrencyCode
         AverageRate
         EndOfDayRate
         ModifiedDate)
  (table Customer
         #:column
         CustomerID
         PersonID
         StoreID
         TerritoryID
         AccountNumber
         rowguid
         ModifiedDate)
  (table PersonCreditCard
         #:column
         BusinessEntityID
         CreditCardID
         ModifiedDate)
  (table SalesOrderDetail
         #:column
         SalesOrderID
         SalesOrderDetailID
         CarrierTrackingNumber
         OrderQty
         ProductID
         SpecialOfferID
         UnitPrice
         UnitPriceDiscount
         LineTotal
         rowguid
         ModifiedDate)
  (table SalesOrderHeader
         #:column
         SalesOrderID
         RevisionNumber
         OrderDate
         DueDate
         ShipDate
         Status
         OnlineOrderFlag
         SalesOrderNumber
         PurchaseOrderNumber
         AccountNumber
         CustomerID
         SalesPersonID
         TerritoryID
         BillToAddressID
         ShipToAddressID
         ShipMethodID
         CreditCardID
         CreditCardApprovalCode
         CurrencyRateID
         SubTotal
         TaxAmt
         Freight
         TotalDue
         Comment
         rowguid
         ModifiedDate)
  (table SalesOrderHeaderSalesReason
         #:column
         SalesOrderID
         SalesReasonID
         ModifiedDate)
  (table SalesPerson
         #:column
         BusinessEntityID
         TerritoryID
         SalesQuota
         Bonus
         CommissionPct
         SalesYTD
         SalesLastYear
         rowguid
         ModifiedDate)
  (table SalesPersonQuotaHistory
         #:column
         BusinessEntityID
         QuotaDate
         SalesQuota
         rowguid
         ModifiedDate)
  (table SalesReason
         #:column
         SalesReasonID
         Name
         ReasonType
         ModifiedDate)
  (table SalesTaxRate
         #:column
         SalesTaxRateID
         StateProvinceID
         TaxType
         TaxRate
         Name
         rowguid
         ModifiedDate)
  (table SalesTerritory
         #:column
         TerritoryID
         Name
         CountryRegionCode
         Group
         SalesYTD
         SalesLastYear
         CostYTD
         CostLastYear
         rowguid
         ModifiedDate)
  (table SalesTerritoryHistory
         #:column
         BusinessEntityID
         TerritoryID
         StartDate
         EndDate
         rowguid
         ModifiedDate)
  (table ShoppingCartItem
         #:column
         ShoppingCartItemID
         ShoppingCartID
         Quantity
         ProductID
         DateCreated
         ModifiedDate)
  (table SpecialOffer
         #:column
         SpecialOfferID
         Description
         DiscountPct
         Type
         Category
         StartDate
         EndDate
         MinQty
         MaxQty
         rowguid
         ModifiedDate)
  (table SpecialOfferProduct
         #:column
         SpecialOfferID
         ProductID
         rowguid
         ModifiedDate)
  (table Store
         #:column
         BusinessEntityID
         Name
         SalesPersonID
         Demographics
         rowguid
         ModifiedDate))

(define (task1/revision1)
  (from subcat ProductSubcategory
        (join cat ProductCategory
              (join-on (.= (ProductCategoryID cat)
                           (ProductCategoryID subcat))))
        (select (Name subcat) #:as 'SubcategoryName)
        (select (Name cat) #:as 'CategoryName)))

(define (task1/revision2)
  (from subcat ProductSubcategory
        (join cat (ProductCategory subcat))
        (select (Name subcat) #:as 'SubcategoryName)
        (select (Name cat) #:as 'CategoryName)))

(define (task1/revision3)
  (from subcat ProductSubcategory
        ; The join is no longer here!
        (select (Name subcat) #:as 'SubcategoryName)
        (select (Name (ProductCategory subcat)) #:as 'CategoryName)))

(define (task1/revision4)
  (from subcat ProductSubcategory
        (select (Name subcat) #:as 'SubcategoryName)
        (select (CategoryName subcat) #:as 'CategoryName)))
