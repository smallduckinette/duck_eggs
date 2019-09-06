#include <iostream>
#include <memory>
#include <btBulletCollisionCommon.h>
#include <btBulletDynamicsCommon.h>


int main()
{
  btDefaultCollisionConfiguration collisionConfiguration;
  btCollisionDispatcher dispatcher(&collisionConfiguration);
  btDbvtBroadphase overlappingPairCache;
  btSequentialImpulseConstraintSolver solver;
  btDiscreteDynamicsWorld world(&dispatcher, &overlappingPairCache, &solver, &collisionConfiguration);
  
  world.setGravity(btVector3(0, -10, 0));
  
  btBoxShape box(btVector3(50, 50, 50));
  btTransform boxTransform;
  boxTransform.setIdentity();
  boxTransform.setOrigin(btVector3(0, -50, 0));
  btDefaultMotionState boxState(boxTransform);
  btRigidBody::btRigidBodyConstructionInfo boxInfo(0,
                                                   &boxState,
                                                   &box,
                                                   btVector3(0, 0, 0));
  btRigidBody boxBody(boxInfo);
  boxBody.setRestitution(0.8);
  world.addRigidBody(&boxBody);
  
  btSphereShape sphere(btScalar(1));  
  btVector3 localInertia(0, 0, 0);  
  sphere.calculateLocalInertia(1, localInertia);
  
  btTransform startTransform;
  startTransform.setIdentity();
  startTransform.setOrigin(btVector3(0, 10, 0));
  
  btDefaultMotionState motionState(startTransform);
  btRigidBody::btRigidBodyConstructionInfo rbInfo(1,
                                                  &motionState,
                                                  &sphere,
                                                  localInertia);
  btRigidBody body(rbInfo);
  body.setLinearVelocity(btVector3(0, 0, 1));
  body.setRestitution(0.9);
  world.addRigidBody(&body);
  
  for(int i = 0; i < 200; ++i)
  {
    world.stepSimulation(1.f/60.f, 10);

    btTransform trans;
    motionState.getWorldTransform(trans);
    std::cout << trans.getOrigin().getX() << ","
              << trans.getOrigin().getY() << ","
              << trans.getOrigin().getZ() << std::endl;
  }
}
