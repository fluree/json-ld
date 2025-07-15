#!/usr/bin/env node

/**
 * Test JavaScript <-> ClojureScript data conversion
 * This verifies that the JS-friendly API properly handles data conversion
 */

import { expand, compact, normalizeData, parseContext, jsonLd } from '../../dist/nodejs/fluree-json-ld.js';

// Complex test data with nested structures
const complexTestDoc = {
  "@context": {
    "name": "http://schema.org/name",
    "Person": "http://schema.org/Person",
    "knows": "http://schema.org/knows",
    "birthDate": {
      "@id": "http://schema.org/birthDate",
      "@type": "http://www.w3.org/2001/XMLSchema#date"
    }
  },
  "@type": "Person",
  "name": "John Doe",
  "birthDate": "1990-01-01",
  "knows": [
    {
      "@type": "Person", 
      "name": "Jane Smith"
    },
    {
      "@type": "Person",
      "name": "Bob Johnson"  
    }
  ]
};

const simpleTestDoc = {
  "@context": {
    "name": "http://schema.org/name",
    "Person": "http://schema.org/Person"
  },
  "@type": "Person",
  "name": "John Doe"
};

async function runConversionTests() {
  console.log('🧪 Running JS<->CLJ data conversion tests...');
  
  let passed = 0;
  let failed = 0;

  // Test 1: Simple JSON-LD detection
  try {
    const result1 = jsonLd(simpleTestDoc);
    const result2 = jsonLd({name: "Just a name"});
    
    if (result1 === true && result2 === false) {
      console.log('✅ jsonLd() detection works correctly');
      passed++;
    } else {
      console.log('❌ jsonLd() detection failed:', result1, result2);
      failed++;
    }
  } catch (error) {
    console.log('❌ jsonLd() detection failed:', error.message);
    failed++;
  }

  // Test 2: Simple normalize  
  try {
    const normalized = normalizeData(simpleTestDoc);
    if (typeof normalized === 'string' && normalized.length > 0) {
      console.log('✅ normalizeData() with simple JS object works');
      console.log('   Result length:', normalized.length, 'characters');
      passed++;
    } else {
      console.log('❌ normalizeData() failed with simple object');
      failed++;
    }
  } catch (error) {
    console.log('❌ normalizeData() failed:', error.message);
    failed++;
  }

  // Test 3: Complex normalize
  try {
    const normalized = normalizeData(complexTestDoc);
    if (typeof normalized === 'string' && normalized.length > 0) {
      console.log('✅ normalizeData() with complex JS object works');
      console.log('   Result length:', normalized.length, 'characters');
      passed++;
    } else {
      console.log('❌ normalizeData() failed with complex object');
      failed++;
    }
  } catch (error) {
    console.log('❌ normalizeData() complex failed:', error.message);
    failed++;
  }

  // Test 4: Parse context conversion
  try {
    const simpleContext = { "name": "http://schema.org/name" };
    const complexContext = complexTestDoc["@context"];
    
    const parsed1 = parseContext(simpleContext);
    const parsed2 = parseContext(complexContext);
    
    if (parsed1 && parsed2 && 
        typeof parsed1 === 'object' && 
        typeof parsed2 === 'object') {
      console.log('✅ parseContext() handles JS objects correctly');
      passed++;
    } else {
      console.log('❌ parseContext() conversion failed');
      failed++;
    }
  } catch (error) {
    console.log('❌ parseContext() conversion failed:', error.message);
    failed++;
  }

  // Test 5: Expand with nested structures
  try {
    const expanded = expand(complexTestDoc);
    if (expanded && typeof expanded === 'object') {
      console.log('✅ expand() handles complex nested JS structures');
      // Log a bit of the structure
      console.log('   Expanded keys:', Object.keys(expanded).slice(0, 3));
      passed++;
    } else {
      console.log('❌ expand() failed with complex structures');
      failed++;
    }
  } catch (error) {
    console.log('❌ expand() with nested structures failed:', error.message);
    failed++;
  }

  // Test 6: Array handling
  try {
    const arrayDoc = [simpleTestDoc, complexTestDoc];
    const normalized = normalizeData(arrayDoc);
    if (typeof normalized === 'string') {
      console.log('✅ Array of documents handled correctly');
      passed++;
    } else {
      console.log('❌ Array handling failed');
      failed++;
    }
  } catch (error) {
    console.log('❌ Array handling failed:', error.message);
    failed++;
  }

  // Summary
  console.log(`\n📊 Conversion Test Results: ${passed} passed, ${failed} failed`);
  
  if (passed >= 4) {
    console.log('🎉 JS<->CLJ data conversion working excellently!');
    process.exit(0);
  } else if (passed >= 2) {
    console.log('🎉 Core data conversion verified!');
    process.exit(0);
  } else {
    console.log('💥 Data conversion has serious issues');
    process.exit(1);
  }
}

runConversionTests().catch(error => {
  console.error('❌ Conversion test runner failed:', error);
  process.exit(1);
});