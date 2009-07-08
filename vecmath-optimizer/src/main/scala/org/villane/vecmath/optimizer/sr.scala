package org.villane.vecmath.optimizer

/**
 * Mark classes with this, otherwise the optimizer will not look into them.
 * 
 * Mark methods with this, otherwise the optimizer will not touch them.
 */
class sr extends StaticAnnotation

/**
 * Mark methods that should not be optimized!
 */
class nosr extends StaticAnnotation